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
      *    ����������̾              COPY (CPPTNYUBYOMEI.INC)    *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  PTNYUBYOMEI-REC.
      *����
           03  PTNYUBYOMEI-KEY.
      *���ŵ��أɣ�
               05  PTNYUBYOMEI-HOSPID              PIC  X(24).
      *���ԣɣ�
               05  PTNYUBYOMEI-PTID                PIC  9(10).
      *���������ֹ����������ǡܣ����ĺ��֡�
               05  PTNYUBYOMEI-RRKNUM              PIC  9(03).
      *��̾������
           03  PTNYUBYOMEI-BYOMEICD-1              PIC  X(07).
           03  PTNYUBYOMEI-BYOMEICD-2              PIC  X(07).
           03  PTNYUBYOMEI-BYOMEICD-3              PIC  X(07).
           03  PTNYUBYOMEI-BYOMEICD-4              PIC  X(07).
           03  PTNYUBYOMEI-BYOMEICD-5              PIC  X(07).
      *������̾
           03  PTNYUBYOMEI-CHARTBYOMEI             PIC  X(160).
      *ü���ɣ�
           03  PTNYUBYOMEI-TERMID                  PIC  X(16).
      *���ڡ��졼���ɣ�
           03  PTNYUBYOMEI-OPID                    PIC  X(16).
      *����ǯ����
           03  PTNYUBYOMEI-CREYMD                  PIC  X(08).
      *����ǯ����
           03  PTNYUBYOMEI-UPYMD                   PIC  X(08).
      *��������
           03  PTNYUBYOMEI-UPHMS                   PIC  X(06).