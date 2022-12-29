import * as Gleam from "./gleam.mjs";
import * as Process from "node:process";
const { watch } = require('node:fs/promises'); // import { watch } from "node:fs/promises";
const fs = require('fs');

export const argv = () => Gleam.List.fromArray(Process.argv.slice(1));

export const cwd = () => Process.cwd();

export const start_file_change_watcher = function (file_change_handler_fn) {
	const base_dir = cwd();
	(async () => {
		const watcher = watch(base_dir, { persistent: true, recursive: true });
		for await (const event of watcher) {
			if (event.eventType == "change") {
				file_change_handler_fn(event.filename);
			}
		}
	})();
};

export const read_file = function (absolute_file_name) {
	try {
		const data = fs.readFileSync(absolute_file_name, 'utf8');
		// console.log(data);
		return data;
	} catch (err) {
		console.error(err);
	}
};

export const file_exists = function (absolute_file_name) {
	if (fs.existsSync(absolute_file_name)) {
		return true;
	}
	return false;
};

const path = require("path");
export const find_files_recursive = function (file_exts_list, directory) {
	file_exts_list = file_exts_list.toArray();
	// console.log(file_exts_list);
	let files = [];
	const getFilesRecursively = (directory) => {
		const filesInDirectory = fs.readdirSync(directory);
		for (const file of filesInDirectory) {
			const absolute = path.join(directory, file);
			if (fs.statSync(absolute).isDirectory()) {
				getFilesRecursively(absolute);
			} else {
				files.push(absolute);
			}
		}
	};
	getFilesRecursively(directory);

	files = files.filter(function(absolute_file_name) {
		return absolute_file_name.endsWith(file_exts_list);
	});

	return Gleam.List.fromArray(files);
};
