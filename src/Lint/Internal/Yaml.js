// @ts-check
import yaml from "js-yaml";

/**
 * One YAML document as JSON, or the parser's own sentence about why
 * not. Every JSON file is a YAML document, so this reads both.
 *
 * `load` returns `undefined` for an empty document, which is not a
 * thing the caller can decode - `null` is, and means the same.
 *
 * @type {(text: string) => { ok: boolean, value: unknown, why: string }}
 */
export const parseImpl = (text) => {
  try {
    const value = yaml.load(text);
    return { ok: true, value: value === undefined ? null : value, why: "" };
  } catch (why) {
    return { ok: false, value: null, why: why instanceof Error ? why.message : String(why) };
  }
};
