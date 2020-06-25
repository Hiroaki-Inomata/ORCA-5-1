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
      *    ���Ը�������                  COPY (CPPTCONF.INC)     *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  PTCONF-REC.
      *����
           03  PTCONF-KEY.
      *���ŵ��أɣ�
               05  PTCONF-HOSPNUM             PIC  9(2).
      *���ԣɣ�
               05  PTCONF-PTID                PIC  9(10).
      *����ˣţ� (Configuration KEY)
               05  PTCONF-CKEY                PIC  X(25).
      *����ǡ��� (Configuration DATA)
           03  PTCONF-CDATA                   PIC  X(10).
      *ü���ɣ�
           03  PTCONF-TERMID                  PIC  X(16).
      *���ڡ��졼���ɣ�
           03  PTCONF-OPID                    PIC  X(16).
      *����ǯ����
           03  PTCONF-CREYMD                  PIC  X(08).
      *����ǯ����
           03  PTCONF-UPYMD                   PIC  X(08).
      *��������
           03  PTCONF-UPHMS                   PIC  X(06).