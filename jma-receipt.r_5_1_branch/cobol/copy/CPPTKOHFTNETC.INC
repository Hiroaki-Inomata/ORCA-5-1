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
      *    ���Ը�����ô               COPY  (CPPTKOHFTNETC.INC)  *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  PTKOHFTNETC-REC.
      *����
           03  PTKOHFTNETC-KEY.
      *���ŵ��أɣ�
               05  PTKOHFTNETC-HOSPNUM        PIC  9(02).
      *���ԣɣ�
               05  PTKOHFTNETC-PTID           PIC  9(10).
      *����ɣ�
               05  PTKOHFTNETC-KOHID          PIC  9(10).
      *ǯ����
               05  PTKOHFTNETC-SRYYMD         PIC  X(08).
      *Ϣ��
               05  PTKOHFTNETC-RENNUM         PIC  9(02).
      *�����ݸ��ֹ�
           03  PTKOHFTNETC-KOHNUM             PIC  X(03).
      *������ʬ
           03  PTKOHFTNETC-NYUGAIKBN          PIC  X(01).
      *��ô���
           03  PTKOHFTNETC-FTNMONEY           PIC  9(07).
      *ü���ɣ�
           03  PTKOHFTNETC-TERMID             PIC  X(16).
      *���ڡ��졼���ɣ�
           03  PTKOHFTNETC-OPID               PIC  X(16).
      *����ǯ����
           03  PTKOHFTNETC-CREYMD             PIC  X(08).
      *����ǯ����
           03  PTKOHFTNETC-UPYMD              PIC  X(08).
      *��������
           03  PTKOHFTNETC-UPHMS              PIC  X(06).
