import globals from "globals";
import js from "@eslint/js";
import eslintPluginPrettierRecommended from "eslint-plugin-prettier/recommended";

export default [
  {
    ignores: [
      // TODO: Use eslint on js lib and generated code

      // Ignore lib/js for now, which uses jshint currently
      "lib/js/*",
      // Ignore all generated code for now
      "**/gen-*/",

      // Don't lint nested node_modules
      "**/node_modules/",
    ],
  },
  js.configs.recommended,
  eslintPluginPrettierRecommended,
  {
    languageOptions: {
      globals: {
        ...globals.node,
      },

      ecmaVersion: 2022,
      sourceType: "commonjs",
    },

    rules: {
      "no-console": "off",
      "no-var": "error",
      "prefer-const": "error",

      "no-constant-condition": [
        "error",
        {
          checkLoops: false,
        },
      ],
    },
  },
  {
    files: ["**/*.mjs"],
    languageOptions: {
      sourceType: "module",
    },
  },
];
