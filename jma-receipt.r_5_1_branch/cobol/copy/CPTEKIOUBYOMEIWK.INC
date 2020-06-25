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
      *  Ŭ����̾�ޥ���       (CPTEKIOUBYOMEIWK.INC)
      *****************************************************************
      *****************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *01  TEKIOUBYOMEIWK-REC.
      *
      *������ʬ
           03  TEKBYOWK-SYORIKBN          PIC X(01).
           03  TEKBYOWK-KEY.
      *�����å���ʬ
               05  TEKBYOWK-CHKKBN        PIC X(01).
      *���Ź԰٥�����
               05  TEKBYOWK-SRYCD         PIC X(09).
      *Ϣ�ֹ�
               05  TEKBYOWK-RENNUM        PIC 9(04).
      *Ŭ����̾
           03  TEKBYOWK-BYOMEI            PIC X(400).
      *��̾�����ɿ�
           03  TEKBYOWK-BYOMEICDSU        PIC 9(02).
      *��̾������
           03  TEKBYOWK-BYOMEICD-G.
               05  TEKBYOWK-BYOMEICD-1    PIC X(07).
               05  TEKBYOWK-BYOMEICD-2    PIC X(07).
               05  TEKBYOWK-BYOMEICD-3    PIC X(07).
               05  TEKBYOWK-BYOMEICD-4    PIC X(07).
               05  TEKBYOWK-BYOMEICD-5    PIC X(07).
               05  TEKBYOWK-BYOMEICD-6    PIC X(07).
               05  TEKBYOWK-BYOMEICD-7    PIC X(07).
               05  TEKBYOWK-BYOMEICD-8    PIC X(07).
               05  TEKBYOWK-BYOMEICD-9    PIC X(07).
               05  TEKBYOWK-BYOMEICD-10   PIC X(07).
               05  TEKBYOWK-BYOMEICD-11   PIC X(07).
               05  TEKBYOWK-BYOMEICD-12   PIC X(07).
               05  TEKBYOWK-BYOMEICD-13   PIC X(07).
               05  TEKBYOWK-BYOMEICD-14   PIC X(07).
               05  TEKBYOWK-BYOMEICD-15   PIC X(07).
               05  TEKBYOWK-BYOMEICD-16   PIC X(07).
               05  TEKBYOWK-BYOMEICD-17   PIC X(07).
               05  TEKBYOWK-BYOMEICD-18   PIC X(07).
               05  TEKBYOWK-BYOMEICD-19   PIC X(07).
               05  TEKBYOWK-BYOMEICD-20   PIC X(07).
               05  TEKBYOWK-BYOMEICD-21   PIC X(07).
      *
           03  TEKBYOWK-BYOMEICD-R        REDEFINES TEKBYOWK-BYOMEICD-G.
               05  TEKBYOWK-BYOMEICD      PIC X(07)
                                          OCCURS   21.
      *��̾�Խ��ե饰
           03  TEKBYOWK-BYOMEIHENFLG      PIC X(01).
      *������̾������
           03  TEKBYOWK-KHNBYOMEICD       PIC X(07).
      *�������̥����ɣ�
           03  TEKBYOWK-KHNBUICD-1        PIC X(07).
      *�������̥����ɣ�
           03  TEKBYOWK-KHNBUICD-2        PIC X(07).
      *�������̥����ɣ�
           03  TEKBYOWK-KHNBUICD-3        PIC X(07).
      *ü���ɣ�
           03  TEKBYOWK-TERMID            PIC X(32).
      *���ڡ��졼���ɣ�
           03  TEKBYOWK-OPID              PIC X(16).
      *����ǯ����
           03  TEKBYOWK-CREYMD            PIC X(08).
      *����ǯ����
           03  TEKBYOWK-UPYMD             PIC X(08).
      *��������
           03  TEKBYOWK-UPHMS             PIC X(06).