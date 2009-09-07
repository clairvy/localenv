-- createtbl.sql
-- Create private_jisyo table from an expanded format of SKK private dictionary.
--
-- NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
-- $Date: 2001/02/03 00:23:00 $:
-- Version: $Id: createtbl.sql,v 1.2 2001/02/03 00:23:00 minakaji Exp $
-- 
-- 
--      % nkf -e ~/.skk-jisyo | gawk -f ./awk/dicconv.awk  - > ./tmp/private.txt
--      % nkf -e /usr/local/share/skk/SKK-JISYO.L | gawk -f ./awk/dicconv.awk  - > ./tmp/large.txt
--      % nkf -e /usr/local/share/skk/SKK-JISYO.JIS2 | gawk -f ./awk/dicconv.awk  - > ./tmp/jis2.txt
--
-- �Ȥ��� ~/.skk-jisyo ��Ÿ������*.txt �ե��������ޤ���
--
--      gawk ��ȤäƤ���Τϡ��ȼ���ǽ�� strftime() ��ȤäƤ��뤿��
--      �ǡ��⤷�� gawk ���ʤ���С�dicconv.awk ��
--          ctime = myctime(0);
--      �Ȥ����Ľ��
--          ctime = 'Sat Aug 15 17:23:45 1998';
--      �Ȥ����褦��ľ�񤭤����б����Ʋ����� (�Ȥꤢ������Ȥ�Ԥʤä�
--      ����Ŭ���ʻ��֤� Emacs �� current-time-string �Υե����ޥåȤ�
--      ������ɤ��Ǥ�������myctime ������⥳���ȥ����Ȥ��ʤ���Ф�
--      ��ʤ����⤷��ޤ���
--
--  PS. ���Υ�����ץ���� gsub ��Ȥ��褦�ˤʤä��Τǡ�ɬ�� gawk ��Ȥ�
-- �Ʋ�������
--
-- ����
--
--      '/usr/local/share/emacs/site-lisp/skk/experimental/rdbms/tmp/private.txt'
--      '/usr/local/share/emacs/site-lisp/skk/experimental/rdbms/tmp/large.txt'
--      '/usr/local/share/emacs/site-lisp/skk/experimental/rdbms/tmp/jis2.txt'
--      '/usr/local/share/emacs/site-lisp/skk/experimental/rdbms/tmp/kakutei.txt'
--
-- ����ʬ������ *.txt �ե�����ν�ߤ˹�碌�ơ��ޤ���
--
--      minakaji_private_jisyo (minakaji_kakutei_jisyo),
--      minakaji_private_yomi_index (minakaji_kakutei_yomi_index)
--
-- ����ʬ��
--
--      YOUR-USER-LOGIN-NAME_private_jisyo (YOUR-USER-LOGIN-NAME_kakutei_jisyo)
--      YOUR-USER-LOGIN-NAME_yomi_index (YOUR-USER-LOGIN-NAME_kakutei_yomi_index)
--
-- �Τ褦�ˤ��ʤ��Υ�����͡��� (Emacs ���ѿ� user-login-name ������
-- �Ƥ�����) ���դ��ä��ƽ�ľ����
--
--   % psql skk -e < createtbl.sql
--
-- �Ȥ��ơ�skk �ǡ����١������äƲ�������
--
-- �ʤ������Υե�����ϡ�postgreSQL �ǻȤ���褦��ɬ�� euc-japan-unix
-- ����¸���Ʋ�������

-- DROP TABLE minakaji_skk_private_jisyo; -- old name
DROP TABLE minakaji_private_jisyo;
CREATE TABLE minakaji_private_jisyo (
	okuriari int2 NOT NULL,
	yomi varchar(50) NOT NULL, -- longest entry of yomi in SKK-JISYO.L is 
				   -- '�ۤ��꤯���󤿤󤫤���������Ĥ������������������'
	kanji text NOT NULL,
	okurigana varchar(4),
	date abstime NOT NULL
);

COPY minakaji_private_jisyo FROM '/usr/local/share/emacs/site-lisp/skk/experimental/rdbms/tmp/private.txt';
-- CREATE INDEX minakaji_private_yomi_index ON minakaji_private_jisyo USING BTREE (yomi varchar_ops);
VACUUM minakaji_private_jisyo;
SELECT COUNT(*) FROM minakaji_private_jisyo;

-- DROP TABLE skk_large_jisyo; -- old name
DROP TABLE large_jisyo;
CREATE TABLE large_jisyo (
	okuriari int2 NOT NULL,
	yomi varchar(50) NOT NULL,
	kanji text NOT NULL
);
COPY large_jisyo FROM '/usr/local/share/emacs/site-lisp/skk/experimental/rdbms/tmp/large.txt';
CREATE INDEX large_yomi_index ON large_jisyo USING BTREE (yomi varchar_ops);
VACUUM large_jisyo;
SELECT COUNT(*) FROM large_jisyo;

-- DROP TABLE skk_jis2_jisyo; -- old name
-- DROP TABLE jis2_jisyo;
-- CREATE TABLE jis2_jisyo (
--	okuriari int2 NOT NULL,
--	yomi varchar(50) NOT NULL,
--	kanji text NOT NULL
-- );
-- COPY jis2_jisyo FROM '/usr/local/share/emacs/site-lisp/skk/experimental/rdbms/tmp/jis2.txt';
-- CREATE INDEX jis2_yomi_index ON jis2_jisyo USING BTREE (yomi varchar_ops);
-- VACUUM jis2_jisyo;
-- SELECT COUNT(*) FROM jis2_jisyo;

-- DROP TABLE minakaji_kakutei_jisyo;
-- CREATE TABLE minakaji_kakutei_jisyo (
-- 	okuriari int2 NOT NULL,
-- 	yomi varchar(50) NOT NULL,
-- 	kanji text NOT NULL
-- 	);
-- COPY minakaji_kakutei_jisyo FROM '/usr/local/share/emacs/site-lisp/skk/experimental/rdbms/tmp/kakutei.txt';
-- CREATE INDEX minakaji_kakutei_yomi_index ON minakaji_kakutei_jisyo USING BTREE (yomi varchar_ops);

-- VACUUM minakaji_kakutei_jisyo;
-- SELECT COUNT(*) FROM minakaji_kakutei_jisyo;

-- end of createtbl.sql
