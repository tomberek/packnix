{
  description = "A packrat/PEG parsing engine in pure Nix, plus grammars for JSON, YAML, TSV, nix flake.lock, ATerm/Nix .drv, and Ruby's Gemfile/Gemfile.lock.";

  outputs =
    { self }:
    {
      lib = {
        packrat = import ./lib/packrat.nix;
        grammars = {
          json = import ./grammar/json.nix;
          yaml = import ./grammar/yaml.nix;
          tsv = import ./grammar/tsv.nix;
          flakelock = import ./grammar/flakelock.nix;
          gemfileLock = import ./grammar/gemfile-lock.nix;
          gemfile = import ./grammar/gemfile.nix;
          aterm = import ./grammar/aterm.nix;
          drv = import ./grammar/drv.nix;
        };
      };
    };
}
