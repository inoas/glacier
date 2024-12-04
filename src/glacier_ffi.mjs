import { SrcModuleKind, TestModuleKind } from "./glacier.mjs";
import * as Gleam from "./gleam.mjs";
import child_process from 'node:child_process';
import fs from "node:fs";
import fs_promises from "node:fs/promises";
import path from "node:path";
import process from 'node:process';

const file_change_watcher_debounce_interval_in_ms = 100;
const Nil = undefined; // Translates to `Nil` in Gleam

['SIGINT', 'SIGTERM', 'SIGQUIT']
  .forEach(signal => process.on(signal, function () {
    console.log("\n🏔 Gracefully shutting down Glacier from SIGINT (Ctrl-C)!");
    process.exit(0);
  }));

process.on('warning', function (e) {
  console.warn(e.stack);
});

export const start_args = function () {
  return Gleam.List.fromArray(process.argv.slice(2));
};

export const cwd = function () {
  return process.cwd();
};

export const start_file_change_watcher = function (file_change_handler_fn) {
  let file_change_handler_timeout_id = null;
  let file_change_handler_collection = [];
  const watch_directory = async function (directory, observed_events, file_change_handler_fn, module_kind) {
    let watcher = undefined;
    if (globalThis.Deno) {
      watcher = Deno.watchFs([directory], { recursive: true });
    } else {
      watcher = fs_promises.watch(directory, { persistent: true, recursive: true });
    }
    for await (const event of watcher) {
      const event_kind = function () {
        if (globalThis.Deno) {
          return event.kind;;
        } {
          return event.eventType;
        }
      }();
      const touched_file = function () {
        if (globalThis.Deno) {
          return event.paths[0];
        } {
          return directory + "/" + event.filename;
        }
      }();
      if (observed_events.includes(event_kind) && touched_file.endsWith(".gleam")) {
        if (file_change_handler_timeout_id !== null) {
          clearTimeout(file_change_handler_timeout_id);
        }
        file_change_handler_collection.push([module_kind, touched_file]);
        file_change_handler_timeout_id = setTimeout(function () {
          // NodeJS fs.watch is prone to report the same change twice, thus we need to distinct the changes:
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
        }, file_change_watcher_debounce_interval_in_ms);
      }
    }
  };
  watch_directory(cwd() + "/src", ["change", "rename", "modify"], file_change_handler_fn, new SrcModuleKind());
  watch_directory(cwd() + "/test", ["change", "rename", "modify"], file_change_handler_fn, new TestModuleKind());

  return Nil;
};

export const read_file = function (absolute_file_name) {
  try {
    const data = fs.readFileSync(absolute_file_name, 'utf8');
    return new Gleam.Ok(data);
  } catch (err) {
    // console.error({"Could not read file" : err});
    return new Gleam.Error(Nil);
  }
};

export const file_exists = function (absolute_file_name) {
  if (fs.existsSync(absolute_file_name)) {
    return true;
  }
  return false;
};

export const find_files_recursive_by_exts = function (directory, file_exts_list) {
  file_exts_list = file_exts_list.toArray();
  let files = [];
  /* mut files */ const detect_files_recursive = function (directory) {
    const files_in_directory = fs.readdirSync(directory);
    for (const file of files_in_directory) {
      let absolute_path = path.join(directory, file);
      absolute_path = absolute_path.replace(/\s/g, '');
      if (fs.statSync(absolute_path).isDirectory()) {
        detect_files_recursive(absolute_path);
      } else if (absolute_path.endsWith(file_exts_list) && file_exists(absolute_path)) {
        files.push(absolute_path);
      }
    }
  };
  detect_files_recursive(directory);
  return Gleam.List.fromArray(files);
};

export const shell_exec_print = async function (gleam_list_of_graphemes) {
  if (globalThis.Deno) {
    Deno.run({
      cmd: ["gleam", ...gleam_list_of_graphemes.toArray()]
    });
  } else {
    const cmd = "gleam " + gleam_list_of_graphemes.toArray().join(" ");
    let { stdout } = await node_shell_exec(cmd);
    for (let line of stdout.split('\n')) {
      console.log(`${line}`);
    }
  }
}

const node_shell_exec = async function (cmd) {
  return new Promise(function (resolve, reject) {
    child_process.exec(cmd, (err, stdout, stderr) => {
      if (err) {
        reject(err);
      } else {
        resolve({ stdout, stderr });
      }
    });
  });
}
