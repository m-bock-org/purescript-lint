// @ts-check
import yaml from "js-yaml";

/**
 * One YAML document as JSON, or the parser's own sentence about why
 * not. Every JSON file is a YAML document, so this reads both.
 *
 * `CORE_SCHEMA` rather than js-yaml's default, which adds timestamps:
 * with it, an unquoted `2026-09-13` comes back as a `Date` object,
 * which is not JSON and cannot be decoded as anything. A configuration
 * file has no business carrying typed dates, and a reader who wrote
 * a day expects the day back.
 *
 * Then through `JSON.parse(JSON.stringify(...))`, so what is returned
 * really is JSON. Without it this claims a type it does not have -
 * anything js-yaml builds that JSON has no word for arrives as a value
 * the decoder cannot describe, and the error names a type rather than
 * the line.
 *
 * `load` returns `undefined` for an empty document, which is not a
 * thing the caller can decode - `null` is, and means the same.
 *
 * @type {(text: string) => { ok: boolean, value: unknown, why: string }}
 */
export const parseImpl = (text) => {
  try {
    const read = yaml.load(text, { schema: yaml.CORE_SCHEMA });
    const value = read === undefined ? null : JSON.parse(JSON.stringify(read));
    return { ok: true, value: value === undefined ? null : value, why: "" };
  } catch (why) {
    return { ok: false, value: null, why: why instanceof Error ? why.message : String(why) };
  }
};
