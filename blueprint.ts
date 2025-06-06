type ParseBlueprint = {
  preamble: {
    title: string;
    description: string;
    version: string;
    plutusVersion: string;
    license: string;
  };
  validators: {
    title: string;
    datum?: {
      title: string;
      schema: {
        $ref: string;
      };
    };
    redeemer: {
      title: string;
      schema: {
        $ref: string;
      };
    };
    parameters?: {
      title: string;
      schema: {
        $ref: string;
      };
    }[];
    plutarchCode?: boolean,
    compiledCode: string;
    hash: string;
  }[];
  definitions: Record<string, {
    title: string;
    schema: {
      $ref: string;
    };
  }>;
};

const plutusJson: ParseBlueprint = JSON.parse(
  await Deno.readTextFile('plutus.json'),
);

const plutusVersion = `Plutus${
  plutusJson.preamble.plutusVersion.toUpperCase()}`;

const { definitions } = plutusJson;

const imports = `// deno-lint-ignore-file
import { applyParamsToScript, Data, Validator, applyDoubleCborEncoding } from "${
  'npm:@lucid-evolution/lucid'
}"`;

const validators = plutusJson.validators.map((validator) => {
  const { title } = validator;
  const name = (() => {
    const dirs = title.split('/')
    const [a, b] = dirs[dirs.length - 1].split('.');
    console.log([a, b])
    return upperFirst(snakeToCamel(a)) + upperFirst(snakeToCamel(b));
  })();
  const { datum } = validator;
  const datumTitle = datum ? snakeToCamel(datum.title) : null;
  const datumSchema = datum ? resolveSchema(datum.schema, definitions) : null;

  const { redeemer } = validator;
  const redeemerTitle = snakeToCamel(redeemer.title);
  const redeemerSchema = resolveSchema(redeemer.schema, definitions);

  const params = validator.parameters || [];
  const plutarchScript = validator.plutarchCode ? true : false;
  const paramsSchema = {
    dataType: 'list',
    items: params.map((param) => resolveSchema(param.schema, definitions)),
  };
  const paramsArgs = params.map((
    param,
    index,
  ) => [snakeToCamel(param.title), schemaToType(paramsSchema.items[index])]);

  const script = validator.compiledCode;

  return `export interface I${name} {
    new (${paramsArgs.map((param) => param.join(':')).join(',')}): Validator;${
  datum ? `\n${datumTitle}: ${schemaToType(datumSchema)};` : ''
}
    ${redeemerTitle}: ${schemaToType(redeemerSchema)};
  };

  export const ${name} = Object.assign(
    function (${paramsArgs.map((param) => param.join(':')).join(',')}) {${
  (paramsArgs.length > 0 && !plutarchScript)
    ? `return { type: "${plutusVersion}", script: applyParamsToScript(applyDoubleCborEncoding("${script}"), [${
      paramsArgs.map((param) => param[0]).join(',')
    }], ${JSON.stringify(paramsSchema)}) };`
    : `return {type: "${plutusVersion}", script: "${script}"};`
}},
    ${datum ? `{${datumTitle}: ${JSON.stringify(datumSchema)}},` : ''}
    {${redeemerTitle}: ${JSON.stringify(redeemerSchema)}},
  ) as unknown as I${name};`;
});

const plutus = `${imports}\n\n${validators.join('\n\n')}`;

await Deno.writeTextFile('plutus.ts', plutus);
await new Deno.Command(Deno.execPath(), {
  args: ['fmt', 'plutus.ts'],
  stderr: 'piped',
}).output();
console.log(
  '%cGenerated %cplutus.ts',
  'color: green; font-weight: bold',
  'font-weight: bold',
);

function resolveSchema(schema: any, definitions: any): any {
  if (schema.items) {
    if (schema.items instanceof Array) {
      return {
        ...schema,
        items: schema.items.map((item: any) => resolveSchema(item, definitions)),
      };
    }
    return {
      ...schema,
      items: resolveSchema(schema.items, definitions),
    };
  } if (schema.anyOf) {
    return {
      ...schema,
      anyOf: schema.anyOf.map((a: any) => ({
        ...a,
        fields: a.fields.map((field: any) => ({
          ...resolveSchema(field, definitions),
          title: field.title ? snakeToCamel(field.title) : undefined,
        })),
      })),
    };
  } if (schema.keys && schema.values) {
    return {
      ...schema,
      keys: resolveSchema(schema.keys, definitions),
      values: resolveSchema(schema.values, definitions),
    };
  }
  if (schema.$ref) {
    const refKey = schema.$ref.replaceAll('~1', '/').split('#/definitions/')[1];
    return resolveSchema(definitions[refKey], definitions);
  }
  return schema;
}

function schemaToType(schema: any): string {
  if (!schema) throw new Error('Could not generate type.');
  const shapeType = (schema.anyOf ? 'enum' : '') || schema.dataType;

  switch (shapeType) {
    case 'integer': {
      return 'bigint';
    }
    case 'bytes': {
      return 'string';
    }
    case 'constructor': {
      if (isVoid(schema)) {
        return 'undefined';
      }
      return `{${
        schema.fields.map((field: any) => `${field.title || 'wrapper'}:${schemaToType(field)}`).join(';')
      }}`;
    }
    case 'enum': {
      // When enum has only one entry it's a single constructor/record object
      if (schema.anyOf.length === 1) {
        return schemaToType(schema.anyOf[0]);
      }
      if (isBoolean(schema)) {
        return 'boolean';
      }
      if (isNullable(schema)) {
        return `${schemaToType(schema.anyOf[0].fields[0])} | null`;
      }
      return schema.anyOf.map((entry: any) => (entry.fields.length === 0
        ? `"${entry.title}"`
        : `{${entry.title}: ${
          entry.fields[0].title
            ? `{${
              entry.fields.map((field: any) => [field.title, schemaToType(field)].join(':')).join(',')
            }}}`
            : `[${
              entry.fields.map((field: any) => schemaToType(field)).join(',')
            }]}`
        }`)).join(' | ');
    }
    case 'list': {
      if (schema.items instanceof Array) {
        return `[${
          schema.items.map((item: any) => schemaToType(item)).join(',')
        }]`;
      }
      return `Array<${schemaToType(schema.items)}>`;
    }
    case 'map': {
      return `Map<${schemaToType(schema.keys)}, ${
        schemaToType(schema.values)
      }>`;
    }
    case undefined: {
      return 'Data';
    }
  }
  throw new Error('Could not type cast data.');
}

function isBoolean(shape: any): boolean {
  return shape.anyOf && shape.anyOf[0]?.title === 'False'
      && shape.anyOf[1]?.title === 'True';
}

function isVoid(shape: any): boolean {
  return shape.index === 0 && shape.fields.length === 0;
}

function isNullable(shape: any): boolean {
  return shape.anyOf && shape.anyOf[0]?.title === 'Some'
      && shape.anyOf[1]?.title === 'None';
}

function snakeToCamel(s: string): string {
  const withUnderscore = s.charAt(0) === '_' ? s.charAt(0) : '';
  return withUnderscore
      + (withUnderscore ? s.slice(1) : s).toLowerCase().replace(
        /([-_][a-z])/g,
        (group) => group
          .toUpperCase()
          .replace('-', '')
          .replace('_', ''),
      );
}

function upperFirst(s: string): string {
  const withUnderscore = s.charAt(0) === '_' ? s.charAt(0) : '';
  return withUnderscore
      + s.charAt(withUnderscore ? 1 : 0).toUpperCase()
      + s.slice((withUnderscore ? 1 : 0) + 1);
}
