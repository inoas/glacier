import { readdir, readFile } from "fs/promises";
import { join as joinPath } from "path";
import * as NodeProcess from "node:process";
import * as Gleam from "./gleam.mjs";

// async function* gleamFiles(directory) {
//   let dirents = await readdir(directory, { withFileTypes: true });
//   for (let dirent of dirents) {
//     let path = joinPath(directory, dirent.name);
//     if (dirent.isDirectory()) {
//       yield* gleamFiles(path);
//     } else if (path.endsWith(".gleam")) {
//       yield path;
//     }
//   }
// }

async function readRootPackageName() {
  let toml = await readFile("gleam.toml", "utf-8");
  for (let line of toml.split("\n")) {
    let matches = line.match(/\s*name\s*=\s*"([a-z][a-z0-9_]*)"/); // Match regexp in compiler-cli/src/new.rs in validate_name()
    if (matches) return matches[1];
  }
  throw new Error("Could not determine package name from gleam.toml");
}

export async function main(test_modules, halts_on_error) {
  let passes = 0;
  let failures = 0;
  const failureMsgs = [];

  let packageName = await readRootPackageName();
  let dist = `../${packageName}/`;

  test_modules = test_modules.toArray();
  // test_modules = await gleamFiles("test");
  // console.log(test_modules)

  for await (let path of test_modules) {
    let js_path = path.slice("test/".length).replace(".gleam", ".mjs");
    let module = await import(joinPath(dist, js_path));
    for (let fnName of Object.keys(module)) {
      if (!fnName.endsWith("_test")) {
        continue;
      }
      try {
        await module[fnName]();
        process.stdout.write(`\u001b[32m.\u001b[0m`);
        passes++;
      } catch (error) {
        process.stdout.write(`\u001b[31mF\u001b[0m`);
        let moduleName = js_path.slice(0, -4);
        let line = error.line ? `:${error.line}` : "";
        failures++;
        failureMsgs.push(`❌ ${failures}) ${moduleName}.${fnName}${line}\n${error}\n`);
      }
    }
  }

	process.stdout.write("\n")
  let result_info = `${passes + failures} tests, ${failures} failures`;
  if (failures == 0) {
    result_info = `\u001b[32m${result_info}\u001b[0m`;
  } else {
		process.stdout.write("Failures:\n\n");
		process.stdout.write(failureMsgs.join("\n") + "\n");
    result_info = `\u001b[31m${result_info}\u001b[0m`;
  }
  console.log(result_info);

  if (halts_on_error) {
    process.exit(failures ? 1 : 0);
  } else {
    process.exit(0);
  }
}

export function crash(message) {
  throw new Error(message);
}

// TODO: Polish and copy to glacier_ffi.mjs
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

  files = files.filter(function (absolute_file_name) {
    return absolute_file_name.endsWith(file_exts_list);
  });

  return Gleam.List.fromArray(files);
};

export const cwd = () => NodeProcess.cwd();

export const file_exists = function (absolute_file_name) {
  if (fs.existsSync(absolute_file_name)) {
    return true;
  }
  return false;
};
