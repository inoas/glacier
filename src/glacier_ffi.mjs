import * as Gleam from "./gleam.mjs";
import * as NodeFs from "node:fs";
import * as NodeFsPromises from "node:fs/promises";
import * as NodePath from "node:path";
import * as NodeProcess from "node:process";
import { SrcModuleKind, TestModuleKind } from "./glacier.mjs";

const file_change_watcher_debounce_interval_in_ms = 100;
const Nil = undefined; // Translates to `Nil` in Gleam

// process.on('SIGINT', function () {
//   process.exit(0);
// });
['SIGINT', 'SIGTERM', 'SIGQUIT']
	.forEach(signal => process.on(signal, function () {
		console.log("\n🏔 Gracefully shutting down Glacier from SIGINT (Ctrl-C)!");
		process.exit(0);
	}));

process.on('warning', function (e) {
	console.warn(e.stack);
});

export const start_args = function () {
	return Gleam.List.fromArray(NodeProcess.argv.slice(1));
};

export const cwd = function () {
	return NodeProcess.cwd();
};

export const start_file_change_watcher = function (file_change_handler_fn) {
	let file_change_handler_timeout_id = null;
	let file_change_handler_collection = [];
	const watch_directory = async function (directory, events, file_change_handler_fn, module_kind) {
		const watcher = NodeFsPromises.watch(directory, { persistent: true, recursive: true });
		for await (const event of watcher) {
			if (events.includes(event.eventType) && event.filename.endsWith(".gleam")) {
				const touched_file = directory + "/" + event.filename
				if (file_change_handler_timeout_id !== null) {
					clearTimeout(file_change_handler_timeout_id);
				}
				file_change_handler_collection.push([module_kind, touched_file]);
				file_change_handler_timeout_id = setTimeout(function () {
					// possibly drop nextTick?
					process.nextTick(function () {
						// NodeJS Fs.watch is prone to report the same change twice, thus we need to distinct the changes:
						let distinct_file_change_handler_collection = [...new Set(file_change_handler_collection)];
						// As we collect file on a delay set by file_change_watcher_debounce_interval_in_ms,
						// they could be gone once we want to handle them:
						distinct_file_change_handler_collection = distinct_file_change_handler_collection.filter(function (file_info) {
							let absolute_file_name = file_info[1];
							absolute_file_name = absolute_file_name.replace(/\s/g, '');
							return file_exists(absolute_file_name);
						});
						if (distinct_file_change_handler_collection.length > 0) {
							file_change_handler_fn(Gleam.List.fromArray(distinct_file_change_handler_collection));
							file_change_handler_timeout_id = null;
							file_change_handler_collection = [];
						}
					});
				}, file_change_watcher_debounce_interval_in_ms);
			}
		}
	};
	watch_directory(cwd() + "/src", ["change", "rename"], file_change_handler_fn, new SrcModuleKind());
	watch_directory(cwd() + "/test", ["change", "rename"], file_change_handler_fn, new TestModuleKind());

	return Nil;
};

export const read_file = function (absolute_file_name) {
	try {
		const data = NodeFs.readFileSync(absolute_file_name, 'utf8');
		return new Gleam.Ok(data);
	} catch (err) {
		// console.error({"Could not read file" : err});
		return new Gleam.Error(Nil);
	}
};

export const file_exists = function (absolute_file_name) {
	if (NodeFs.existsSync(absolute_file_name)) {
		return true;
	}
	return false;
};

export const find_files_recursive_by_exts = function (directory, file_exts_list) {
	file_exts_list = file_exts_list.toArray();
	let files = [];
  /* mut files */ const detect_files_recursive = function (directory) {
		const files_in_directory = NodeFs.readdirSync(directory);
		for (const file of files_in_directory) {
			let absolute_path = NodePath.join(directory, file);
			absolute_path = absolute_path.replace(/\s/g, '');
			if (NodeFs.statSync(absolute_path).isDirectory()) {
				detect_files_recursive(absolute_path);
			} else if (absolute_path.endsWith(file_exts_list) && file_exists(absolute_path)) {
				files.push(absolute_path);
			}
		}
	};
	detect_files_recursive(directory);
	return Gleam.List.fromArray(files);
};
