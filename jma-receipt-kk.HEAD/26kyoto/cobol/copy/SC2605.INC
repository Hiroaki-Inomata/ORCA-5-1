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
       01  SC05.
      *����ǯ��
         03  SC05-PRTYM             PIC  X(16).
      *����ǯ����
         03  SC05-SEIYMD            PIC  X(22).
      *����
         03  SC05-ADRS              PIC  X(100).
      *���ŵ���̾
         03  SC05-HOSPNAME          PIC  X(100).
      *������̾
         03  SC05-KAISETUNAME       PIC  X(60).
      *���ŵ��إ�����
         03  SC05-HOSPCD            PIC  X(07).
      *����ɽ��ʬ
         03  SC3-TENSUKBN-TBL OCCURS 3.
             05  SC05-TENSUKBN      PIC X(02).
      *��̾
         03  SC05-KANANAME-TBL  OCCURS  20.
             05  SC05-KANANAME      PIC  X(20).
         03  SC05-NAME-TBL      OCCURS  20.
             05  SC05-NAME          PIC  X(20).
      *�ݸ����̣�
         03  SC05-HKNKBN-TBL   OCCURS  20.
             05  SC05-HKNKBN-G OCCURS  6.
                 07  SC05-HKNKBN    PIC  X(02).
      *������ô���ֹ�
         03  SC05-FTNJANUM-TBL   OCCURS  20.
             05  SC05-FTNJANUM      PIC  X(08).
      *������ֹ�
         03  SC05-JKYSNUM-TBL   OCCURS  20.
             05  SC05-JKYSNUM       PIC  X(14).
      *����ǯ��
         03  SC05-SRYYM-TBL   OCCURS  20.
             05  SC05-SRYYM         PIC  X(08).
      *�ݸ�������
         03  SC05-TENSU-TBL     OCCURS  20.
             05  SC05-TENSU         PIC  X(09).
      *����ʬ������ô��
         03  SC05-KOHFTN-TBL   OCCURS  20.
             05  SC05-KOHFTN        PIC  X(06).
      *������ô
         03  SC05-ITBFTN-TBL    OCCURS  20.
             05  SC05-ITBFTN       PIC  X(05).
      *�ܿͲ�²��ʬ
         03  SC05-HONKZK-TBL    OCCURS 20.
             05  SC05-HONKZKKBN     PIC  X(01).
      *���
         03  SC05-GOKEI-TBL.
      *��׷��
             05  SC05-GKENSU        PIC  X(03).
      *�������
             05  SC05-GTENSU        PIC  X(09).
      *��׸���ʬ������ô��
             05  SC05-GKOHFTN       PIC  X(07).
      *��׼�����ô
             05  SC05-GITBFTN       PIC  X(06).
      *�õ�
      *  ��
         03  SC05-TOKKI1-TBL OCCURS 20.
             05  SC05-TOKKI1 OCCURS 2 PIC X(02).
      *  Ĺ����
         03  SC05-TOKKI2-TBL OCCURS 20.
             05  SC05-TOKKI2 OCCURS 2 PIC X(04).
