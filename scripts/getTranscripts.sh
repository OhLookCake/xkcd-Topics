for i in {1..1266}
do
	urli="http://xkcd.com/"$i"/"
	wget $urli
	l=`cat index.html | grep "Permanent link to this comic: " | cut -d\  -f6 | cut -d\< -f1`
	trans=`sed -e '/<div id="transcript" style="display: none">/,/<\/div>/!d' -e 's_</div>$__g' -e 's_^<div id="transcript" style="display: none">__g' index.html | sed ':a;N;$!ba;s/\n/ | /g' |  perl -MHTML::Entities -ne 'print decode_entities($_)'`

	echo -e $l"\t"$trans >> ../data/transcripts.csv

	rm -f index.html
	echo $i" done"
done

