import test from "tape";
import { IncludeTest as IncludeTestEs5 } from "./gen-nodejs/Include_types.js";
import { IncludeTest as IncludeTestEs6 } from "./gen-nodejs-es6/Include_types.js";
import { IncludeTest as IncludeTestEsm } from "./gen-nodejs-esm/Include_types.mjs";

function constructTest(classVariant) {
  return function (t) {
    const obj = new classVariant({ bools: { im_true: true, im_false: false } });

    t.assert(obj.bools.im_true === true);
    t.assert(obj.bools.im_false === false);
    t.end();
  };
}

test("construct es5", constructTest(IncludeTestEs5));
test("construct es6", constructTest(IncludeTestEs6));
test("construct esm", constructTest(IncludeTestEsm));
