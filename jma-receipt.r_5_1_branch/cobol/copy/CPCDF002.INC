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
      ************************************************************
      *    �����ǡ����ʿ��ž����         COPY (CPCDF002.INC)    *
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.05.00    NACL-��ƣ    10/01/28  ���̾���������
      ************************************************************
      *01  COLDAT02-REC.
           03  COLDAT02-KEY.
               05  COLDAT02-KEY1.
      *            ���ŵ��أɣ�
                   07  COLDAT02-HOSPID     PIC  X(24).
      *            ����ǯ��
                   07  COLDAT02-SRYYM      PIC  9(06).
      *            ������ʬ
                   07  COLDAT02-NYUGAIKBN  PIC  X(01).
      *            ���ԣɣ�
                   07  COLDAT02-PTID       PIC  9(10).
      *            �쥻�ץȿ��Ų�
                   07  COLDAT02-RECEKA     PIC  X(02).
      *            ���ݸ��ݸ��ֹ�
                   07  COLDAT02-HKNNUM     PIC  X(03).
      *            ����ɣ�
                   07  COLDAT02-KOHID      PIC  9(10).
      *            �ݸ����ֹ�        
                   07  COLDAT02-HKNJANUM   PIC  X(08).
      *            �ܿͲ�²��ʬ
                   07  COLDAT02-HONKZKKBN  PIC  X(01).
      *            �����ʬ
                   07  COLDAT02-HOJOKBN    PIC  X(01).
      *        ���Ų�
               05  COLDAT02-SRYKA          PIC  X(02).
      *        ���Ŷ�ʬ
               05  COLDAT02-SRYKBN         PIC  X(02).
      *        ���ֹ�
               05  COLDAT02-ZAINUM         PIC  9(08).
      *        Ϣ��
               05  COLDAT02-SEQNO          PIC  9(03).
      *Ϣ�ֹ�
           03  COLDAT02-RENNUM             PIC  9(02).
      *�ݸ��ȹ礻
           03  COLDAT02-HKNCOMBI           PIC  9(04).
      *
      *���ݸ����ݸ��ɣ�
           03  COLDAT02-HKNID              PIC  9(10).
      *����ɣ�
           03  COLDAT02-KOHID-AREA.
               05  COLDAT02-TKOHID         PIC  9(10)   OCCURS  4.
      *�쥻�ſ��Ų�
           03  COLDAT02-RECESRYKA          PIC  X(02).
      *���ݶ�ʬ
           03  COLDAT02-JIHOKBN            PIC  X(01).
      *������
           03  COLDAT02-ZAITEN             PIC  9(08).
      *�޲��
           03  COLDAT02-ZAIKAISU           PIC  9(04).
      *���ٿ�
           03  COLDAT02-MEISAISU           PIC  9(04).
      *�굻������
           03  COLDAT02-SYUTEN1            PIC  9(07).
      *�굻������
           03  COLDAT02-SYUTEN2            PIC  9(07).
      *��������
           03  COLDAT02-YKZTEN             PIC  9(07).
      *�������
           03  COLDAT02-KIZAITEN           PIC  9(07).
      *��������ơ��֥�
           03  COLDAT02-DAY-AREA.
      *        ���Ų���ơ��֥�
               05  COLDAT02-KAISUINF       OCCURS  31.
                   07  COLDAT02-KAISU      PIC  9(03).
      *���Ź԰پ���
           03  COLDAT02-SRYACTINF.
      *���ż��̶�ʬ
               05  COLDAT02-SRYSYUKBN      PIC  X(03).
      *���ť�����
               05  COLDAT02-SRYCD          PIC  X(09).
      *����
               05  COLDAT02-SRYSURYO       PIC  9(05)V9(05).
      *���
               05  COLDAT02-SRYKAISU       PIC  9(03).
      *�������̶�ʬ
               05  COLDAT02-TENSIKIBETU    PIC  9(01).
      *����
               05  COLDAT02-TEN            PIC  9(09)V9(02).
      *̾�������ֹ�
               05  COLDAT02-INPUTNUM       PIC  9(03).
      *������
               05  COLDAT02-JIHIMONEY      PIC  9(07).
      *ñ�̥�����
               05  COLDAT02-TANICD         PIC  X(03).
      *ñ��̾��
               05  COLDAT02-TANINAME       PIC  X(24).
      *����̾��
               05  COLDAT02-NAME           PIC  X(200).
      *����̾��
               05  COLDAT02-FORMALNAME     PIC  X(400).
      *������ॳ����
               05  COLDAT02-YAKKAKJNCD     PIC  X(12).
      *�����󽸷��輱��
               05  COLDAT02-TENTTLKBN      PIC  9(03).
