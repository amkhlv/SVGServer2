import 'package:diff_match_patch/diff_match_patch.dart';
import 'dart:io';
import 'dart:convert';

void main(List<String> arguments) {
  var old = stdin
      .transform(utf8.decoder)
      .transform(const LineSplitter());
  var stdinAcc = StringBuffer();
  old.listen(
      (line) => stdinAcc.writeln(line),
      onDone: () {
        var oldsvg = stdinAcc.toString();
        var newfile = File(arguments[0]);
        var dmp = DiffMatchPatch();
        newfile.readAsString().then((String newsvg) {
          var d = dmp.diff(oldsvg, newsvg);
          var result = patchToText(patchMake(d)).trim();
          // Empty line is not a valid patch, so we have to check:
          if (result.isNotEmpty) print(patchToText(patchMake(d)).trim());
        });
      },
  );
}

