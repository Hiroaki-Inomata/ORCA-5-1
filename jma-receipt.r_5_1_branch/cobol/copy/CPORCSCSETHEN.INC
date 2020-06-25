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
      *    ���Ź԰٥��å�Ÿ���ѥ�᥿����COPY (CPORCSCSETHEN.INC)
      *                           ���ѥץ������ (ORCGK02,ORCGK02N)
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSCSETHENAREA.
      *
      *�ץ������ɣ�
           03  ORCSCSETHEN-PG                  PIC  X(08).
      *���å�̾
           03  ORCSCSETHEN-SRYCD               PIC  X(09).
      *�оݹ�
           03  ORCSCSETHEN-IDX                 PIC  9(04).
      *�����Խ���
           03  ORCSCSETHEN-SC92-AREA.
      *        ����
               05  ORCSCSETHEN-SYORIFLG          PIC 9(01).
      *        ǯ�� 
               05  ORCSCSETHEN-NENREI.
                   07  ORCSCSETHEN-NENREI-YY     PIC 9(03).
                   07  ORCSCSETHEN-NENREI-MM     PIC 9(02).
                   07  ORCSCSETHEN-NENREI-DD     PIC 9(02).
      *        ������̤���ƿǲʻ���ͭ��ʽ�ǡ��ƿǤΤ߻��ѡ�
               05  ORCSCSETHEN-15MIMAN-ARI       PIC  9(01).
      *        Ϸ�Ͷ�ʬ
               05  ORCSCSETHEN-ROUJIN            PIC 9(01).
      *        �ǽ����ǲ�
               05  ORCSCSETHEN-LSTSRYKA          PIC  X(02).
      *        �ǽ����ǲ�
               05  ORCSCSETHEN-LSTSRYKAMEI       PIC  X(20).
      *        �������ο��ť�����
               05  ORCSCSETHEN-TEISEI-AREA.
                   07  ORCSCSETHEN-TEISEI-OCC      OCCURS   100.
                       09  ORCSCSETHEN-TEI-SRYCD   PIC X(09).
                       09  ORCSCSETHEN-TEI-KAISU   PIC 9(03).
      *    ���顼��å�����
           03  ORCSCSETHEN-ERRMSG2        PIC  X(100).
      *