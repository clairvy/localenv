BEGIN{
  OFS = "\t";
}
{
if ($3 != "") {
  printf("%s\t%s\t", $1, $2);
  if ($3 == "����(�Ԥ�)") print "0";
  else if ($3 == "���Ը���") print "1";
  else if ($3 == "���Ը���") print "2";
  else if ($3 == "����(����)") print "3";
  else if ($3 == "����(����)&̾��") print "3", "44";
  else if ($3 == "����(����)&̾�첽������") print "3", "36";
  # "3" �� 3 �Ĥˤʤä����� "4","5" �Ϸ��֡�ľ�������ݤ����
  else if ($3 == "���Ը���") print "6";
  else if ($3 == "����(����)") print "7";
  else if ($3 == "���Ը���") print "8";
  else if ($3 == "�ʹԸ���") print "9";
  else if ($3 == "�йԸ���") print "10";
  else if ($3 == "�޹Ը���") print "11";
  else if ($3 == "���(������)") print "12";
  else if ($3 == "��Ը���") print "13";
  else if ($3 == "��Ը���") print "14";
  else if ($3 == "����") print "15";
  else if ($3 == "����&̾��") print "15", "44";
  # "16" �Ϸ��֡�
  else if ($3 == "��ư��") print "17";
  else if ($3 == "����") print "18";
  else if ($3 == "���ƻ�") print "19";
  else if (match($3, /���ƻ첽����/) != 0) print "20";
  else if ($3 == "����ư��") print "21";
  else if (($3 == "����ư��&̾��") || ($3 == "����ư��,̾��")) print "21", "44";
  # sort ���� uniq ����Ȳ��Τ������Ĥ��ФƤ��ʤ���
  else if ($3 == "���ƻ첽����ư��") print "22";
  else if ($3 == "����ư��(����)") print "23";
  else if ($3 == "����ư�첽������") print "24";
  else if ($3 == "��ͭ̾��") print "25";
  else if ($3 == "������") print "26";
  else if ($3 == "��̾") print "27";
  else if ($3 == "��̾&��̾") print "27", "42";
  # "28" �Ϸ��֡�
  else if ($3 == "����") print "29";
  else if ($3 == "��³��,��ư��") print "30";
  # "��³��" �� "��³��" �ϰ㤦�Τ���
  else if ($3 == "��Ƭ��") print "31";
  else if ($3 == "��Ƭ��(��)") print "32";
  # �Ȥꤢ����  
  else if (match($3, /��Ƭ��\(��/) != 0) print "31";
  else if ($3 == "��Ƭ������") print "33";
  else if ($3 == "��Ƭ����") print "34";
  else if ($3 == "��Ƭ��̾") print "35";
  else if ($3 == "������") print "36";
  else if ($3 == "����������") print "37";
  else if ($3 == "������̾") print "38";
  else if ($3 == "������̾") print "39";
  else if ($3 == "����ư��") print "40";
  else if ($3 == "ñ����") print "41";
  else if ($3 == "��̾") print "42";
  else if ($3 == "����") print "43";
  else if ($3 == "̾��") print "44";
  else if ($3 == "��(��)") print "45";
  else if ($3 == "��(��)") print "46";
  else if ($3 == "��(��)") print "47";
  else if ($3 == "Ϣ�λ�") print "48";
  # sort ���� uniq ����Ȳ��Τ������Ĥ� 3 �Ĥ��ФƤ��ʤ���
  else if ($3 == "��(��)") print "49";
  else if ($3 == "��(��)") print "50";
  else if ($3 == "��(��)") print "51";
}
}
