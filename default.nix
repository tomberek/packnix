let
  evalAttrs =
    string: handlers:
    let
      resolveString =
        name: rule: derivs:
        derivs.${rule} or null;
      resolveList =
        name: rule: derivs:
        let
          result =
            builtins.foldl'
              (
                acc: next:
                let
                  res = if acc ? derivs then resolveAny name next acc.derivs else null;
                in
                if res ? derivs then
                  {
                    value = acc.value or [ ] ++ [ res.value ];
                    derivs = res.derivs;
                  }
                else
                  null
              )
              {
                value = [ ];
                derivs = derivs;
              }
              rule;
        in
        if result == null then
          null
        else
          {
            # value = builtins.concatStringsSep "" result.value or "";
            value = (handlers.${name} or (_: _)) result.value;
            # value = result.value;
            derivs = result.derivs or null;
          };
      resolveSet =
        name: rule: derivs:
        {
          lit =
            let
              m = builtins.substring derivs.count (builtins.stringLength rule.lit) string;
            in
            if rule.lit == m then
              {
                value = rule.lit;
                derivs = derivs.self (derivs.count + (builtins.stringLength (rule.lit)));
              }
            else
              null;

          range =
            let
              value = builtins.substring derivs.count 1 string;
              start = builtins.elemAt rule.range 0;
              end = builtins.elemAt rule.range 1;
            in
            if value >= start && value <= end then
              {
                value = value;
                derivs = derivs.self (derivs.count + 1);
              }
            else
              null;
          choice = builtins.foldl' (
            acc: next:
            if acc != null then
              acc
            else
              let
                res = resolveAny name next derivs;
              in
              if res ? value then
                {
                  value = res.value;
                  derivs = res.derivs;
                }
              else
                null
          ) null rule.choice;
          regex =
            let
              v = builtins.substring derivs.count 128 string;
              m = builtins.match "${rule.regex}.*" v;
            in
            if builtins.isList m then
              {
                # value = builtins.head m;
                value = (handlers.${name} or (_: _)) (builtins.head m);
                derivs = derivs.self (derivs.count + (builtins.stringLength (builtins.head m)));
              }
            else
              null;

          __functor = self: rule: self.${builtins.head (builtins.attrNames rule)};
        }
          rule;

      resolveAny = {
        string = resolveString;
        list = resolveList;
        set = resolveSet;
        __functor =
          self: name: rule: d:
          self.${builtins.typeOf rule} name rule d;
      };

      len = builtins.stringLength string;
      string' = string;
    in
    rules:
    let
      recurse =
        self: count:
        let
          d =
            {
              self = self;
              count = count;
              "" = {
                value = "";
                derivs = d;
              };
            } // builtins.mapAttrs (n: rule: resolveAny n rule d) rules;
        in
        d;
    in
    count:
    let
      x = recurse x;
      result = x count;
    in
    {
      X = result.X.value or false;
    };

  myrun =
    count: string:
    evalAttrs string
      {
        WHITESPACE = v: if (builtins.isList v) then builtins.concatStringsSep "" v else "";
        STRING_RAW = v: builtins.concatStringsSep "" v;
        STRING = v: { string = builtins.elemAt v 1; };
        X = v: builtins.elemAt v 1;
        ITEM = v: {
          name = (builtins.elemAt v 1).string;
          value = (builtins.elemAt v 4);
        };
        NUMBER = builtins.fromJSON;
        LIST =
          v:
          let
            items = builtins.elemAt v 1;
            recurse =
              v:
              if builtins.isList v && builtins.length v == 3 then
                [ (builtins.elemAt v 0) ] ++ (recurse (builtins.elemAt v 2))
              else
                [ v ];
          in
          recurse items;
        SET =
          v:
          let
            items = builtins.elemAt v 1;
            recurse =
              v:
              (
                if builtins.isAttrs v then [ v ] else [ (builtins.elemAt v 0) ] ++ (recurse (builtins.elemAt v 2))
              );
            result = recurse items;

          in
          builtins.listToAttrs result;

      }

      # A JSON Grammar w/ additions!
      {
        X = [ "WHITESPACE" { choice = [
                "SET"
                "LIST"
                "STRING"
                "NUMBER"
                "BOOL"
                "NULL"
        ]; } "WHITESPACE" ];

        WHITESPACE = { choice = [ [ { regex = "([[:space:]]+)"; } "WHITESPACE" ] "" ]; };
        NULL = { lit = "null"; };
        BOOL = { choice = [ { lit = "true"; } { lit = "false"; } ]; };
        NUMBER = { regex = "([0-9]+)"; };

        STRING = [ { lit = "\""; } "STRING_RAW" { lit = "\""; } ];
        STRING_FRAG = { choice = [ { regex = ''([^\\\"]+)''; } { lit = ''\"''; } { lit = ''\''; } ]; };
        STRING_RAW = { choice = [ [ "STRING_FRAG" "STRING_RAW" ] "STRING_FRAG" "" ]; };

        COMMENT = [
          "WHITESPACE"
          { lit = "#"; }
          { regex = "([^\n]+)"; }
          "WHITESPACE"
        ];

        LIST = [ { lit = "["; } "LIST_ITEMS" { lit = "]"; } ];
        LIST_ITEMS = { choice = [ [ "X" { lit = ","; } "LIST_ITEMS" ] "X" ]; };

        SET = [ { lit = "{"; } "ITEMS" { lit = "}"; } ];
        ITEMS = { choice = [ [ "ITEM" { lit = ","; } "ITEMS" ] "ITEM" ]; };
        ITEM = {
          choice = [
            [ "WHITESPACE" "STRING" "WHITESPACE" { lit = ":"; } "X" ]
            [ "COMMENT" "STRING" "WHITESPACE" { lit = ":"; } "X" ]
          ];
        };

      }





      count;
in

# API: give us a file
file:
let
  file = builtins.unsafeDiscardStringContext (builtins.readFile ./lock.json);
  len = builtins.stringLength file;
in
(myrun 0 file).X
