#include <QCoreApplication>
#include <QTextStream>
#include <QtCore>
#include <QString>
#include <QList>
#include <QMap>
#include <QVariant>
#include <QDebug>
#include "diff-match-patch/cpp/diff_match_patch.h"

QTextStream& qStdOut()
{
    static QTextStream ts( stdout );
    return ts;
}

int main(int argc, char **argv) {
   QCoreApplication a(argc, argv);
   diff_match_patch dmp;

   QTextStream qtin(stdin) ;
   QString old("");
   while(!qtin.atEnd()) {
      old.append(qtin.readLine());
      old.append("\n");
   }

   QString nu("");
   QFile f(argv[1]);
   f.open(QIODevice::ReadOnly);
   QTextStream qtfile(&f);
   while(!qtfile.atEnd()) {
       nu.append(qtfile.readLine());
       nu.append("\n");
   }
   f.close();

   QString strPatch = dmp.patch_toText(dmp.patch_make(old, nu));

   QTextStream(stdout) << strPatch << endl ;

   //return a.exec();
 }


