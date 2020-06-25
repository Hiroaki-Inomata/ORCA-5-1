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
      *****************************************************************
      *    �����å��ޥ�����       COPY (CPCHKSND.INC)                   *
      *****************************************************************
      *****************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *01  CHKSND-REC.
           03  CHKSND-KEY.
               05  CHKSND-HOSPNUM        PIC 9(2).
      *�����å���ʬ
               05  CHKSND-CHKKBN         PIC  X(01).
      *���ť�����
               05  CHKSND-SRYCD          PIC  X(09).
      *ͭ������ǯ����
               05  CHKSND-YUKOSTYMD      PIC  X(08).
      *ͭ����λǯ����
               05  CHKSND-YUKOEDYMD      PIC  X(08).
      *�����ɶ�ʬ
               05  CHKSND-CDKBN          PIC  X(01).
      *Ϣ�ֹ�
               05  CHKSND-RENNUM         PIC  9(05).
      *Ŭ�ѥ�����
           03  CHKSND-CD                 PIC  X(170).
      *��̾
           03  CHKSND-BYOMEI             PIC  X(160).
      *����ʬ
           03  CHKSND-COMPKBN            PIC  X(1).
      *������ʬ
           03  CHKSND-UTAGAIKBN          PIC  X(1).
      *��̾�Խ��ե饰
           03  CHKSND-BYOMEIHENFLG       PIC  X(1).
      *������ʬ
           03  CHKSND-NYUGAIKBN          PIC  X(1).
      *���롼�ץ����å���ʬ
           03  CHKSND-GRPCHKKBN          PIC  X(1).
      *���롼�ףɣ�
           03  CHKSND-GRPID              PIC  X(1).
      *��­������
           03  CHKSND-HOSOKU             PIC  X(40).
      *��­�����Ⱦ���ʬ
           03  CHKSND-HOSOKU-COMPKBN     PIC  X(1).
      *Ŭ����̾�����å���ʬ
           03  CHKSND-TEKBYOKBN          PIC  X(1).
      *ü���ɣ�
           03  CHKSND-TERMID             PIC  X(16).
      *���ڡ��졼���ɣ�
           03  CHKSND-OPID               PIC  X(16).
      *����ǯ����
           03  CHKSND-CREYMD             PIC  X(08).
      *����ǯ����
           03  CHKSND-UPYMD              PIC  X(08).
      *�������� 
           03  CHKSND-UPHMS              PIC  X(06).
      *�祭�������ѹ���
           03  CHKSND-UPKEY.
               05  CHKSND-UPCHKSNDKBN      PIC  X(01).
               05  CHKSND-UPSRYCD        PIC  X(09).
               05  CHKSND-UPYUKOSTYMD    PIC  X(08).
               05  CHKSND-UPYUKOEDYMD    PIC  X(08).
               05  CHKSND-UPCDKBN        PIC  X(01).
               05  CHKSND-UPRENNUM       PIC  9(03).
      *
      *�����ѹ���
           03  CHKSND-SELNAME            PIC  X(200).
           03  CHKSND-SELCDSYU           PIC X(1).
           03  CHKSND-SELINPUTCD         PIC X(20).