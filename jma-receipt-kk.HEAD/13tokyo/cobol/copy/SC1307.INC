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
       01  SC07.
      *���ŵ��إ�����
              03  SC07-HOSPCDN         PIC  X(20).
      *����ǯ��
              03  SC07-PRTYM           PIC  X(16).
      *����ǯ����
              03  SC07-SEIYMD          PIC  X(22).
      *������ʬɽ�������
              03  SC07-TENHYOKBN1      PIC  X(02).
      *������ʬɽ��������
              03  SC07-TENHYOKBN2      PIC  X(02).
      *������ʬɽ����Ĵ��
              03  SC07-TENHYOKBN3      PIC  X(02).
      *������ʬɽ����ˬ��
              03  SC07-TENHYOKBN4      PIC  X(02).
      *����
              03  SC07-ADRS            PIC  X(100).
      *���ŵ���̾
              03  SC07-HOSPNAME        PIC  X(100).
      *�����ֹ�
              03  SC07-TEL             PIC  X(15).
      *������̾
              03  SC07-KAISETUNAME     PIC  X(40).
      *��ô���ֹ�
              03  SC07-FTNJANUM1-TBL   OCCURS  10.
                  05  SC07-FTNJANUM1   PIC  X(2).
              03  SC07-FTNJANUM2-TBL   OCCURS  10.
                  05  SC07-FTNJANUM2   PIC  X(8).
      *������ֹ�
              03  SC07-JKYSNUM-TBL   OCCURS  10.
                  05  SC07-JKYSNUM     PIC  X(14).
      *�ݸ����ֹ�
              03  SC07-HKNJANUM-TBL   OCCURS  10.
                  05  SC07-HKNJANUM    PIC  X(16).
      *����Ի�̾
              03  SC07-KANANAME-TBL  OCCURS  10.
                  05  SC07-KANANAME    PIC  X(20).
              03  SC07-NAME-TBL      OCCURS  10.
                  05  SC07-NAME        PIC  X(20).
      *���ճ��
              03  SC07-KYURATE1-TBL OCCURS  10.
                  05  SC07-KYURATE1  PIC  X(2).
              03  SC07-KYURATE2-TBL  OCCURS  10.
                  05  SC07-KYURATE2  PIC  X(2).
              03  SC07-KYURATE3-TBL  OCCURS  10.
                  05  SC07-KYURATE3  PIC  X(2).
      *������ʬ
              03  SC07-NYU-TBL OCCURS  10.
                  05  SC07-NYUGAIKBN1  PIC  X(2).
              03  SC07-GAI-TBL  OCCURS  10.
                  05  SC07-NYUGAIKBN2  PIC  X(2).
      *��������
              03  SC07-NISSU-TBL  OCCURS  10.
                  05  SC07-NISSU       PIC  X(2).
      *�����
              03  SC07-SEIKYU-TBL   OCCURS  10.
                  05  SC07-SEIKYU	PIC  X(7).
      *����������������ôʬ
              03  SC07-SYKYKZFTN-TBL     OCCURS  10.
                  05  SC07-SYKYKZFTN   PIC  X(6).
      *������
              03  SC07-TENSU-TBL     OCCURS  10.
                  05  SC07-TENSU       PIC  X(7).
      *������ô��
              03  SC07-ITBFTN-TBL    OCCURS   10.
                  05  SC07-ITBFTN	PIC  X(6).
      *��׷��
              03  SC07-SKENSU		PIC  X(2).
      *��������
              03  SC07-SSEIKYU		PIC  X(7).
      *�������ܡ�����Ƚ�̵���
              03  SC07-SYKYKZ-TBL OCCURS 10.
                  05  SC07-SYKYKZ       PIC  X(02).
      *���ͥơ��֥�
              03  SC07-BIKO-TBL.
      *����ʸ�����ꥢ
                  05  SC07-MOJI-TBL OCCURS 10.
      *������ʸ������
                      07  SC07-MOJI1    PIC  X(04).
      *������ʸ������
                      07  SC07-MOJI2    PIC  X(04).
      *���͢����ꥢ
                 05  SC07-MARU-TBL OCCURS 10.
      *���͢�����
                      07  SC07-MARU1    PIC  X(02).
      *���͢�����
                      07  SC07-MARU2    PIC  X(02).
