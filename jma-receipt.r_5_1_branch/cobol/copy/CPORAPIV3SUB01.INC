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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      ******************************************************************
      *    ���Уɣ֣���������λ���֥ѥ�᥿
      *                                         (CPORAPIV3SUB01.INC)
      ******************************************************************
       01  ORAPIV3SUB01AREA.
      *    9:������λ
           03  APIV3SUB01-SYORI             PIC X(01).
      *    TERMID��API+���륫�գɣġ�
           03  APIV3SUB01-TERMID            PIC X(64).
      *    ����ƣգɣ�
           03  APIV3SUB01-KARTE-UID         PIC X(36).
      *    �����ֹ�
           03  APIV3SUB01-PTNUM             PIC X(20).
      *    API��̳ID
           03  APIV3SUB01-API-GYOUMUID      PIC X(08).
           03  APIV3SUB01-API-FILEMEI       PIC X(20).
      *    ��̳ID(��¾�����
           03  APIV3SUB01-PARA-GYOUMUID     PIC X(04).
      *    ��̳ID��ͽ����
           03  APIV3SUB01-PARA-GYOUMUID2    PIC X(04).
      *
      *�ֵ�
           03  APIV3SUB01-ERRCD             PIC X(02).
           03  APIV3SUB01-ERRMSG            PIC X(200).
      *
