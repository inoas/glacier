import * as Gleam from "./gleam.mjs";
import * as Process from "node:process";
// import { watch } from "node:fs/promises";
const { watch } = require('node:fs/promises');

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
