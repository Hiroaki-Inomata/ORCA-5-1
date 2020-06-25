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
      *    ������Ģ�ǡ���Ϣ��      COPY (CPOKUSURIKANRI.INC)     *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  OKUSURI-KANRI-REC.
      *����
           03  OKUKAN-KEY.
      *���ŵ��أɣ�
               05  OKUKAN-HOSPNUM           PIC  9(02).
      *���ԣɣ�
               05  OKUKAN-PTID              PIC  9(10).
      *Ĵ����
               05  OKUKAN-SRYYMD            PIC  X(08).
      *���ŵ��إ�����
               05  OKUKAN-HOSPCD            PIC  X(07).
      *Ϣ��
               05  OKUKAN-RENNUM            PIC  9(03).
      *���Ի�̾
           03  OKUKAN-NAME                  PIC  X(100).
      *����
           03  OKUKAN-SEX                   PIC  X(1).
      *��ǯ����
           03  OKUKAN-BIRTHDAY              PIC  X(8).
      *���ŵ���̾
           03  OKUKAN-HOSPNAME              PIC  X(200).
      *�ǡ�������
           03  OKUKAN-DATA-KEY              PIC  9(16).
      *ü���ɣ�
           03  OKUKAN-TERMID                PIC  X(16).
      *���ڡ��졼���ɣ�
           03  OKUKAN-OPID                  PIC  X(16).
      *����ǯ����
           03  OKUKAN-CREYMD                PIC  X(08).
      *����ǯ����
           03  OKUKAN-UPYMD                 PIC  X(08).
      *��������
           03  OKUKAN-UPHMS                 PIC  X(06).