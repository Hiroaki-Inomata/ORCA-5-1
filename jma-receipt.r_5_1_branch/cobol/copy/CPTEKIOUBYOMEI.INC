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
      *  Ŭ����̾�ޥ���       (CPTEKIOUBYOMEI.INC)
      *****************************************************************
      *****************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *01  TEKIOUBYOMEI-REC.
      *
           03  TEKBYO-KEY.
      *�����å���ʬ
               05  TEKBYO-CHKKBN          PIC X(01).
      *���Ź԰٥�����
               05  TEKBYO-SRYCD           PIC X(09).
      *Ϣ�ֹ�
               05  TEKBYO-RENNUM          PIC 9(04).
      *Ŭ����̾
           03  TEKBYO-BYOMEI              PIC X(400).
      *��̾�����ɿ�
           03  TEKBYO-BYOMEICDSU          PIC 9(02).
      *��̾������
           03  TEKBYO-BYOMEICD-G.
               05  TEKBYO-BYOMEICD-1      PIC X(07).
               05  TEKBYO-BYOMEICD-2      PIC X(07).
               05  TEKBYO-BYOMEICD-3      PIC X(07).
               05  TEKBYO-BYOMEICD-4      PIC X(07).
               05  TEKBYO-BYOMEICD-5      PIC X(07).
               05  TEKBYO-BYOMEICD-6      PIC X(07).
               05  TEKBYO-BYOMEICD-7      PIC X(07).
               05  TEKBYO-BYOMEICD-8      PIC X(07).
               05  TEKBYO-BYOMEICD-9      PIC X(07).
               05  TEKBYO-BYOMEICD-10     PIC X(07).
               05  TEKBYO-BYOMEICD-11     PIC X(07).
               05  TEKBYO-BYOMEICD-12     PIC X(07).
               05  TEKBYO-BYOMEICD-13     PIC X(07).
               05  TEKBYO-BYOMEICD-14     PIC X(07).
               05  TEKBYO-BYOMEICD-15     PIC X(07).
               05  TEKBYO-BYOMEICD-16     PIC X(07).
               05  TEKBYO-BYOMEICD-17     PIC X(07).
               05  TEKBYO-BYOMEICD-18     PIC X(07).
               05  TEKBYO-BYOMEICD-19     PIC X(07).
               05  TEKBYO-BYOMEICD-20     PIC X(07).
               05  TEKBYO-BYOMEICD-21     PIC X(07).
      *
           03  TEKBYO-BYOMEICD-R          REDEFINES   TEKBYO-BYOMEICD-G.
               05  TEKBYO-BYOMEICD        PIC X(07)
                                          OCCURS   21.
      *��̾�Խ��ե饰
           03  TEKBYO-BYOMEIHENFLG        PIC X(01).
      *������̾������
           03  TEKBYO-KHNBYOMEICD         PIC X(07).
      *�������̥����ɣ�
           03  TEKBYO-KHNBUICD-1          PIC X(07).
      *�������̥����ɣ�
           03  TEKBYO-KHNBUICD-2          PIC X(07).
      *�������̥����ɣ�
           03  TEKBYO-KHNBUICD-3          PIC X(07).
      *ü���ɣ�
           03  TEKBYO-TERMID              PIC X(32).
      *���ڡ��졼���ɣ�
           03  TEKBYO-OPID                PIC X(16).
      *����ǯ����
           03  TEKBYO-CREYMD              PIC X(08).
      *����ǯ����
           03  TEKBYO-UPYMD               PIC X(08).
      *��������
           03  TEKBYO-UPHMS               PIC X(06).
      *
      *�����ѹ���
           03  TEKBYO-SELNAME             PIC  X(200).
           03  TEKBYO-SELYUKOSTYMD        PIC  X(8).
           03  TEKBYO-SELYUKOEDYMD        PIC  X(8).