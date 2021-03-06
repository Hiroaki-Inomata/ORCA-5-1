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
      *----------------------------------------------------------------*
      *   ��ϥӥꥳ���ɥơ��֥��ϫ�ҳ��� COPY (CMRIHATBL2016.INC)
      *----------------------------------------------------------------*
      *****************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 04.08.00     NACL-¿��Ǽ  16/09/14  �ȣ���ǯ����������б�
      * 05.00.00     NACL-¿��Ǽ  19/03/14  �ȣ���ǯ�����ѻߥ������б�
      *****************************************************************
      *    ��ϥӥ�Υ�����
       01  TBL-RIHABIRI2016-AREA.
      *    ����ɼ��¥�ϥӥ�
           03  FILLER              PIC X(13)   VALUE   "1 1 180027410".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "1 2 180027510".
           03  FILLER              PIC X(01)   VALUE   SPACE.
      *    Ǿ��ɼ��¥�ϥӥ�
           03  FILLER              PIC X(13)   VALUE   "2 1 180027610".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "2 2 180027710".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "2 3 180030810".
           03  FILLER              PIC X(01)   VALUE   SPACE.
      *    Ǿ��ɼ��¥�ϥӥ���ײ���������
           03  FILLER              PIC X(13)   VALUE   "211 180033910".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "212 180034110".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "213 180034310".
           03  FILLER              PIC X(01)   VALUE   "1".
      *    Ǿ��ɼ��¥�ϥӥ���ײ��˴����Ŭ���
           03  FILLER              PIC X(13)   VALUE   "211 180043430".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "212 180043630".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "213 180043830".
           03  FILLER              PIC X(01)   VALUE   "1".
      *    Ǿ��ɼ��¥�ϥӥ���ײ�������
           03  FILLER              PIC X(13)   VALUE   "211 180044310".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "212 180044410".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "213 180044510".
           03  FILLER              PIC X(01)   VALUE   SPACE.
      *    ��ư���ϥӥ�
           03  FILLER              PIC X(13)   VALUE   "3 1 180032710".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "3 2 180027810".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "3 2 180027910".
           03  FILLER              PIC X(01)   VALUE   SPACE.
      *    ��ư���ϥӥ���ײ���������
           03  FILLER              PIC X(13)   VALUE   "311 180034510".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "312 180034610".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "312 180034710".
           03  FILLER              PIC X(01)   VALUE   "1".
      *    ��ư���ϥӥ���ײ��˴����Ŭ��
           03  FILLER              PIC X(13)   VALUE   "311 180044030".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "312 180044130".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "312 180044230".
           03  FILLER              PIC X(01)   VALUE   "1".
      *    ��ư���ϥӥ���ײ�������
           03  FILLER              PIC X(13)   VALUE   "311 180045810".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "312 180045910".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "312 180046010".
           03  FILLER              PIC X(01)   VALUE   SPACE.
      *    �Ƶ۴��ϥӥ�
           03  FILLER              PIC X(13)   VALUE   "4 1 180028010".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "4 2 180028110".
           03  FILLER              PIC X(01)   VALUE   SPACE.
      *    ���Ѿɸ���
           03  FILLER              PIC X(13)   VALUE   "9 1 180044610".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "9 2 180044710".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "9 2 180044810".
           03  FILLER              PIC X(01)   VALUE   SPACE.
      *    ���Ѿɸ������ײ�������
           03  FILLER              PIC X(13)   VALUE   "911 180044910".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "912 180045110".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "913 180045310".
           03  FILLER              PIC X(01)   VALUE   SPACE.
      *    ���Ѿɸ������ײ���������
           03  FILLER              PIC X(13)   VALUE   "911 180045010".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "912 180045210".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "913 180045410".
           03  FILLER              PIC X(01)   VALUE   "1".
      *    ���Ѿɸ������ײ��˴����Ŭ��
           03  FILLER              PIC X(13)   VALUE   "911 180045530".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "912 180045630".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "913 180045730".
           03  FILLER              PIC X(01)   VALUE   "1".
      *
      *H28.19 ����
      *    Ǿ��ɼ��¥�ϥӥ�ʥ긺��
           03  FILLER              PIC X(13)   VALUE   "2 1G180050330".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "2 2G180050430".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "2 3G180050530".
           03  FILLER              PIC X(01)   VALUE   SPACE.
      *    Ǿ��ɼ��¥�ϥӥ���ײ��������ˡʥ긺��
           03  FILLER              PIC X(13)   VALUE   "211G180050730".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "212G180050930".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "213G180051130".
           03  FILLER              PIC X(01)   VALUE   "1".
      *    Ǿ��ɼ��¥�ϥӥ���ײ��˴����Ŭ�硦�긺��
           03  FILLER              PIC X(13)   VALUE   "211G180051230".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "212G180051330".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "213G180051430".
           03  FILLER              PIC X(01)   VALUE   "1".
      *    Ǿ��ɼ��¥�ϥӥ���ײ������ˡʥ긺��
           03  FILLER              PIC X(13)   VALUE   "211G180050630".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "212G180050830".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "213G180051030".
           03  FILLER              PIC X(01)   VALUE   SPACE.
      *    ��ư���ϥӥ�ʥ긺��
           03  FILLER              PIC X(13)   VALUE   "3 1G180052730".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "3 2G180052830".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "3 2G180052930".
           03  FILLER              PIC X(01)   VALUE   SPACE.
      *    ��ư���ϥӥ���ײ��������ˡʥ긺��
           03  FILLER              PIC X(13)   VALUE   "311G180053130".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "312G180053330".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "312G180053530".
           03  FILLER              PIC X(01)   VALUE   "1".
      *    ��ư���ϥӥ���ײ��˴����Ŭ�硦�긺
           03  FILLER              PIC X(13)   VALUE   "311G180053630".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "312G180053730".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "312G180053830".
           03  FILLER              PIC X(01)   VALUE   "1".
      *    ��ư���ϥӥ���ײ������ˡʥ긺��
           03  FILLER              PIC X(13)   VALUE   "311G180053030".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "312G180053230".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "312G180053430".
           03  FILLER              PIC X(01)   VALUE   SPACE.
      *    ���Ѿɸ����ʥ긺��
           03  FILLER              PIC X(13)   VALUE   "9 1G180051530".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "9 2G180051630".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "9 2G180051730".
           03  FILLER              PIC X(01)   VALUE   SPACE.
      *    ���Ѿɸ������ײ������ˡʥ긺��
           03  FILLER              PIC X(13)   VALUE   "911G180051830".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "912G180052030".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "913G180052230".
           03  FILLER              PIC X(01)   VALUE   SPACE.
      *    ���Ѿɸ������ײ��������ˡʥ긺��
           03  FILLER              PIC X(13)   VALUE   "911G180051930".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "912G180052130".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "913G180052330".
           03  FILLER              PIC X(01)   VALUE   "1".
      *    ���Ѿɸ������ײ��˴����Ŭ�硦�긺
           03  FILLER              PIC X(13)   VALUE   "911G180052430".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "912G180052530".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "913G180052630".
           03  FILLER              PIC X(01)   VALUE   "1".
      *
       01  TBL-RIHABIRI2016-AREA-R REDEFINES   TBL-RIHABIRI2016-AREA.
           03  TBL-RIHABIRI2016-OCC    OCCURS  76   INDEXED   TBL-RIH16.
      *        ��ϥӥ����
               05  TBL-RIH16-KBN        PIC X(01).
      *        1:�ײ��
               05  TBL-RIH16-KIG        PIC X(01).
      *        1:(1),2:(2),3:(3)
               05  TBL-RIH16-KBN2       PIC X(01).
      *        G:����
               05  TBL-RIH16-YOBO       PIC X(01).
               05  TBL-RIH16-SRYCD      PIC X(09).
      *H31.3
      *        1:H31.4����в������б�
               05  TBL-RIH16-EDCHK      PIC X(01).
      *
       01  TBL-RIHA2016-MAX             PIC 9(02)  VALUE    76.
      *
