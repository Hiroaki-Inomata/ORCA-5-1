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
      *         �����å��ޥ�����    COPY   (CPCHK004.INC) *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *KANRICD
      *1102 �����å���ǽ����
      *1103 �ǡ��������å����θġ��Υ����å��ˤĤ��ƤΤ��롿���ʤ�����
      *1200 ���󥨥顼����
      *1201 ���󥨥顼����ξܺ�
      *2000 ���̻ؼ����ܾ���
      *2001 �������򴵼�
      *2002 �����ǧ����
       01  CHK004-REC.
      *����
           03  CHK004-KEY.
               05  CHK004-HOSPNUM          PIC 9(2).
      *����������
               05  CHK004-KANRICD          PIC  X(04).
      *��ʬ������
               05  CHK004-KBNCD            PIC  X(08).
      *����ͭ��ǯ����
               05  CHK004-STYUKYMD         PIC  X(08).
      *��λͭ��ǯ����
               05  CHK004-EDYUKYMD         PIC  X(08).
      *���ԣɣ�
           03  CHK004-PTID                 PIC  9(10).
      *�����ơ��֥�
           03  CHK004-KANRITBL             PIC  X(500).
      *ü���ɣ�
           03  CHK004-TERMID               PIC  X(16).
      *���ڡ��졼���ɣ�
           03  CHK004-OPID                 PIC  X(16).
      *����ǯ����
           03  CHK004-CREYMD               PIC  X(08).
      *����ǯ����
           03  CHK004-UPYMD                PIC  X(08).
      *��������
           03  CHK004-UPHMS                PIC  X(06).
      *�����祭�������ѹ���
           03  CHK004-UPDKEY.
               05  CHK004-UPHOSPNUM        PIC 9(2).
               05  CHK004-UPKANRICD        PIC X(04).
               05  CHK004-UPKBNCD          PIC X(08).
               05  CHK004-UPSTYUKYMD       PIC X(08).
               05  CHK004-UPEDYUKYMD       PIC X(08).
           03  CHK004-SELLIMIT             PIC 9(08).
           03  CHK004-SELOFFSET            PIC 9(08).