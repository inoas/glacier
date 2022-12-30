import * as Fs from "node:fs";
import * as FsPromises from "node:fs/promises";
import * as Gleam from "./gleam.mjs";
import * as NodeProcess from "node:process";
import * as Path from "node:path";

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
  watch_directory(cwd() + "/src", ["change"], file_change_handler_fn);
  watch_directory(cwd() + "/test", ["change"], file_change_handler_fn);
};

async function watch_directory(directory, events, file_change_handler_fn) {
  const watcher = FsPromises.watch(directory, { persistent: true, recursive: true });
  for await (const event of watcher) {
    if (events.includes(event.eventType)) {
      let touched_file = directory + "/" + event.filename
      console.log(touched_file);
      file_change_handler_fn(touched_file);
    }
  }
};

export const read_file = function (absolute_file_name) {
  // try {
  const data = Fs.readFileSync(absolute_file_name, 'utf8');
  return data;
  // } catch (err) {
  //   console.error(err);
  // }
};

export const file_exists = function (absolute_file_name) {
  if (Fs.existsSync(absolute_file_name)) {
    return true;
  }
  return false;
};

export const find_files_recursive_by_exts = function (directory, file_exts_list) {
  file_exts_list = file_exts_list.toArray();
  let files = [];
  /* mut files */ const detect_files_recursive = function (directory) {
    const files_in_directory = Fs.readdirSync(directory);
    for (const file of files_in_directory) {
      const absolute_path = Path.join(directory, file);
      if (Fs.statSync(absolute_path).isDirectory()) {
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
