import * as Gleam from "./gleam.mjs";
import * as NodeFs from "node:fs";
import * as NodeFsPromises from "node:fs/promises";
import * as NodePath from "node:path";
import * as NodeProcess from "node:process";

process.on('SIGINT', function () {
  console.log("\n🏔 Gracefully shutting down Glacier from SIGINT (Ctrl-C)!");
  process.exit(0);
});

process.on('warning', function (e) {
  console.warn(e.stack);
});

// require('events')
// const emitter = new events.EventEmitter()
// emitter.setMaxListeners(1000)
// or 0 to turn off the limit
// emitter.setMaxListeners(0)

export const start_args = function () {
  return Gleam.List.fromArray(NodeProcess.argv.slice(1));
};

export const cwd = function () {
  return NodeProcess.cwd();
};

export const start_file_change_watcher = function (file_change_handler_fn) {
	const watch_directory = async function (directory, events, file_change_handler_fn) {
		const watcher = NodeFsPromises.watch(directory, { persistent: true, recursive: true });
		for await (const event of watcher) {
			if (events.includes(event.eventType)) {
				const touched_file = directory + "/" + event.filename
				console.log(touched_file);
				file_change_handler_fn(touched_file);
			}
		}
	};
  watch_directory(cwd() + "/src", ["change"], file_change_handler_fn);
  watch_directory(cwd() + "/test", ["change"], file_change_handler_fn);
};


export const read_file = function (absolute_file_name) {
  // try {
  const data = NodeFs.readFileSync(absolute_file_name, 'utf8');
  return data;
  // } catch (err) {
  //   console.error(err);
  // }
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
      const absolute_path = NodePath.join(directory, file);
      if (NodeFs.statSync(absolute_path).isDirectory()) {
        detect_files_recursive(absolute_path);
      } else {
        files.push(absolute_path);
      }
    }
  };
  detect_files_recursive(directory);
  files = files.filter(function (absolute_file_name) {
    return absolute_file_name.endsWith(file_exts_list);
  });
  return Gleam.List.fromArray(files);
};
