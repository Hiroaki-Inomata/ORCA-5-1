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
      *    �Ҳ�����        �� COPY (CPSYKRRK.INC)     *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  SYKRRK-REC.
      *����
           03  SYK-KEY.
      *���ŵ��أɣ�
               05  SYK-HOSPNUM              PIC 9(2).
      *���ԣɣ�
               05  SYK-PTID                PIC  9(10).
      *�Ҳ��ʬ
               05  SYK-SYKKBN              PIC  X(01).
      *�Ҳ�ǯ����
               05  SYK-SYKYMD              PIC  X(08).
      *�Ҳ���ŵ��أɣ�
           03  SYK-SYKIRYID                PIC  X(10).
      *�Ҳ���ŵ���̾��
           03  SYK-SYKIRYNAME              PIC  X(100).
      *�Ҳ��
           03  SYK-SYKKA                   PIC  X(02).
      *�ӣϣ���
           03  SYK-SOAP                    PIC  X(100).
      *ü���ɣ�
           03  SYK-TERMID                  PIC  X(16).
      *���ڥ졼���ɣ�
           03  SYK-OPID                    PIC  X(16).
      *����ǯ����
           03  SYK-CREYMD                  PIC  X(08).
      *����ǯ����
           03  SYK-UPYMD                   PIC  X(08).
      *��������
           03  SYK-UPHMS                   PIC  X(06).
