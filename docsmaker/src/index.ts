import fg from 'fast-glob';
import fs from 'fs';
import assert from 'assert';
import * as cheerio from 'cheerio';
import { log } from 'console';

console.log(process.argv);

const dir = process.argv[2];
assert(dir, "dir should be given");
assert(fs.existsSync(dir));

const htmls = fg.stream(dir + "/**/*.html");

for await (const f of htmls) {
  log(f);
  const buffer = fs.readFileSync(f);

  const $ = cheerio.loadBuffer(buffer);

  const $title = $('h1').first();

  log($title.text());

  const icons = $title.prev('.icon').children('.micon').toArray().map(x => x.attribs.class.replace('micon', '').trim())
  log(icons);
  break;
}

