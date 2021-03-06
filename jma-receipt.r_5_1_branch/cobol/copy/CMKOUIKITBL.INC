      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *    �������԰��Ź���Ϣ���ֹ�ơ��֥�   COPY (CMKOUIKITBL.INC)
      *****************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
       01  CONST-KOUIKI-AREA.
           03  FILLER              PIC X(02)   VALUE   "01".
           03  FILLER              PIC X(08)   VALUE   "39010004".
           03  FILLER              PIC X(100)  VALUE
                                   "�̳�ƻ�������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "02".
           03  FILLER              PIC X(08)   VALUE   "39020003".
           03  FILLER              PIC X(100)  VALUE
                                   "�Ŀ����������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "03".
           03  FILLER              PIC X(08)   VALUE   "39030002".
           03  FILLER              PIC X(100)  VALUE
                                   "��긩�������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "04".
           03  FILLER              PIC X(08)   VALUE   "39040001".
           03  FILLER              PIC X(100)  VALUE
                                   "�ܾ븩�������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "05".
           03  FILLER              PIC X(08)   VALUE   "39050000".
           03  FILLER              PIC X(100)  VALUE
                                   "���ĸ��������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "06".
           03  FILLER              PIC X(08)   VALUE   "39060009".
           03  FILLER              PIC X(100)  VALUE
                                   "�������������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "07".
           03  FILLER              PIC X(08)   VALUE   "39070008".
           03  FILLER              PIC X(100)  VALUE
                                   "ʡ�縩�������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "08".
           03  FILLER              PIC X(08)   VALUE   "39080007".
           03  FILLER              PIC X(100)  VALUE
                                   "��븩�������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "09".
           03  FILLER              PIC X(08)   VALUE   "39090006".
           03  FILLER              PIC X(100)  VALUE
                                   "���ڸ��������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "10".
           03  FILLER              PIC X(08)   VALUE   "39100003".
           03  FILLER              PIC X(100)  VALUE
                                   "���ϸ��������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "11".
           03  FILLER              PIC X(08)   VALUE   "39110002".
           03  FILLER              PIC X(100)  VALUE
                                   "��̸��������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "12".
           03  FILLER              PIC X(08)   VALUE   "39120001".
           03  FILLER              PIC X(100)  VALUE
                                   "���ո��������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "13".
           03  FILLER              PIC X(08)   VALUE   "39130000".
           03  FILLER              PIC X(100)  VALUE
                                   "����Ը������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "14".
           03  FILLER              PIC X(08)   VALUE   "39140009".
           03  FILLER              PIC X(100)  VALUE
                                   "������������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "15".
           03  FILLER              PIC X(08)   VALUE   "39150008".
           03  FILLER              PIC X(100)  VALUE
                                   "���㸩�������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "16".
           03  FILLER              PIC X(08)   VALUE   "39160007".
           03  FILLER              PIC X(100)  VALUE
                                   "�ٻ����������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "17".
           03  FILLER              PIC X(08)   VALUE   "39170006".
           03  FILLER              PIC X(100)  VALUE
                                   "����������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "18".
           03  FILLER              PIC X(08)   VALUE   "39180005".
           03  FILLER              PIC X(100)  VALUE
                                   "ʡ�温�������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "19".
           03  FILLER              PIC X(08)   VALUE   "39190004".
           03  FILLER              PIC X(100)  VALUE
                                   "�������������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "20".
           03  FILLER              PIC X(08)   VALUE   "39200001".
           03  FILLER              PIC X(100)  VALUE
                                   "Ĺ��������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "21".
           03  FILLER              PIC X(08)   VALUE   "39210000".
           03  FILLER              PIC X(100)  VALUE
                                   "���츩�������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "22".
           03  FILLER              PIC X(08)   VALUE   "39220009".
           03  FILLER              PIC X(100)  VALUE
                                   "�Ų����������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "23".
           03  FILLER              PIC X(08)   VALUE   "39230008".
           03  FILLER              PIC X(100)  VALUE
                                   "���θ��������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "24".
           03  FILLER              PIC X(08)   VALUE   "39240007".
           03  FILLER              PIC X(100)  VALUE
                                   "���Ÿ��������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "25".
           03  FILLER              PIC X(08)   VALUE   "39250006".
           03  FILLER              PIC X(100)  VALUE
                                   "���츩�������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "26".
           03  FILLER              PIC X(08)   VALUE   "39260005".
           03  FILLER              PIC X(100)  VALUE
                                   "�����ܸ������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "27".
           03  FILLER              PIC X(08)   VALUE   "39270004".
           03  FILLER              PIC X(100)  VALUE
                                   "����ܸ������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "28".
           03  FILLER              PIC X(08)   VALUE   "39280003".
           03  FILLER              PIC X(100)  VALUE
                                   "ʼ�˸��������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "29".
           03  FILLER              PIC X(08)   VALUE   "39290002".
           03  FILLER              PIC X(100)  VALUE
                                   "���ɸ��������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "30".
           03  FILLER              PIC X(08)   VALUE   "39300009".
           03  FILLER              PIC X(100)  VALUE
                                   "�²λ����������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "31".
           03  FILLER              PIC X(08)   VALUE   "39310008".
           03  FILLER              PIC X(100)  VALUE
                                   "Ļ�踩�������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "32".
           03  FILLER              PIC X(08)   VALUE   "39320007".
           03  FILLER              PIC X(100)  VALUE
                                   "�纬���������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "33".
           03  FILLER              PIC X(08)   VALUE   "39330006".
           03  FILLER              PIC X(100)  VALUE
                                   "�������������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "34".
           03  FILLER              PIC X(08)   VALUE   "39340005".
           03  FILLER              PIC X(100)  VALUE
                                   "���縩�������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "35".
           03  FILLER              PIC X(08)   VALUE   "39350004".
           03  FILLER              PIC X(100)  VALUE
                                   "�������������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "36".
           03  FILLER              PIC X(08)   VALUE   "39360003".
           03  FILLER              PIC X(100)  VALUE
                                   "���縩�������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "37".
           03  FILLER              PIC X(08)   VALUE   "39370002".
           03  FILLER              PIC X(100)  VALUE
                                   "����������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "38".
           03  FILLER              PIC X(08)   VALUE   "39380001".
           03  FILLER              PIC X(100)  VALUE
                                   "��ɲ���������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "39".
           03  FILLER              PIC X(08)   VALUE   "39390000".
           03  FILLER              PIC X(100)  VALUE
                                   "���θ��������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "40".
           03  FILLER              PIC X(08)   VALUE   "39400007".
           03  FILLER              PIC X(100)  VALUE
                                   "ʡ�����������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "41".
           03  FILLER              PIC X(08)   VALUE   "39410006".
           03  FILLER              PIC X(100)  VALUE
                                   "���츩�������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "42".
           03  FILLER              PIC X(08)   VALUE   "39420005".
           03  FILLER              PIC X(100)  VALUE
                                   "Ĺ�긩�������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "43".
           03  FILLER              PIC X(08)   VALUE   "39430004".
           03  FILLER              PIC X(100)  VALUE
                                   "���ܸ��������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "44".
           03  FILLER              PIC X(08)   VALUE   "39440003".
           03  FILLER              PIC X(100)  VALUE
                                   "��ʬ���������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "45".
           03  FILLER              PIC X(08)   VALUE   "39450002".
           03  FILLER              PIC X(100)  VALUE
                                   "�ܺ긩�������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "46".
           03  FILLER              PIC X(08)   VALUE   "39460001".
           03  FILLER              PIC X(100)  VALUE
                                   "�����縩�������԰��Ź���Ϣ��".
      *
           03  FILLER              PIC X(02)   VALUE   "47".
           03  FILLER              PIC X(08)   VALUE   "39470000".
           03  FILLER              PIC X(100)  VALUE
                                   "���츩�������԰��Ź���Ϣ��".
      *
       01  CONST-KOUIKI-AREA-R     REDEFINES   CONST-KOUIKI-AREA.
           03  CONST-KOUIKI-OCC    OCCURS   47.
               05  CONST-KOUIKI-PREFNUM
                                   PIC X(02).
               05  CONST-KOUIKI-NUM
                                   PIC X(08).
               05  CONST-KOUIKI-NAME
                                   PIC X(100).
      *
       01  CONST-KOUIKI-MAX        PIC 9(02)   VALUE   47.

