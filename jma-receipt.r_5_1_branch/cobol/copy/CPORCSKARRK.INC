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
      *-------------------------------------------------*
      *    ���Ų����򹹿��ѥ�᥿                       *
      *                         COPY  (CPORCSKARRK.INC) *
      *-------------------------------------------------*
       01  ORCSKARRKAREA.
      *
      *�о�ǯ��
           03  SKARRK-SRYYM              PIC  X(06).
      *�ǽ�������
           03  SKARRK-LASTYMD            PIC  X(08).
      *���Ų�
           03  SKARRK-SRYKA              PIC  X(02).
      *����������
           03  SKARRK-RC                 PIC S9(9)   BINARY.
      *���顼̾��
           03  SKARRK-ERRMSG             PIC  X(80).
      *
