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
      *----------------------------------------------------------*
      *    ����ӣУ��Խ��ѥѥ�᥿��   COPY (CPORCSC95.INC)
      *
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *
       01  ORCSC95AREA.
      *�ɣ�
      *    ������ʬ�ʣ����ӣУ��ν���Խ��Τ�
      *              �����ӣУ��ν���Խ���������Ź԰٤κ���
      *              ����������Ź԰٤κ�����
      *              �����������Υ�����Ź԰٤κ�����
      *              ����������Ź԰����Ƥ��ɲ�
           03  LNK-SC95-KBN            PIC X(01).
      *    �����У�̾
           03  LNK-SC95-PG             PIC X(03).
      *    ������
           03  LNK-SC95-SRYYMD         PIC X(08).
           03  LNK-SC95-SRYYMDG        PIC X(14).
      *    �о���������
           03  LNK-SC95-RRKYMD         PIC X(08).
           03  LNK-SC95-RENNUM         PIC 9(01).
           03  LNK-SC95I-DOUJI-RENNUM  PIC 9(01).
      *
           03  LNK-SC95-GRP-DENPNUM   PIC 9(07).
      *    ���Ųʡ��ݸ��ȹ礻�ʥ�����Ź԰��ɲû���
           03  LNK-SC95-SRYKA          PIC X(02).
           03  LNK-SC95-HKNCOMBI       PIC X(04).
      *
      *�ϣգ�
      *    ��³��ʣ����ˣ����Ƿ�³��λ����ˣ����ء�
           03  LNK-SC95-DOUJI.
               05  LNK-SC95-DOUJI-RENNUM    PIC 9(01).
               05  LNK-SC95-DOUJI-DENPNUM   PIC 9(07).
               05  LNK-SC95-DOUJI-HKNNUM    PIC X(04)
                                                   OCCURS   3.
           03   LNK-SC95-LAST-RENNUM   PIC 9(01).
      *    ����ʬ�ι�����������������ѡ�
           03   LNK-SC95-GOK-TENSU     PIC 9(11).
      *    ��ũ��̵ͭ�����������ѡ�
           03   LNK-SC95-TENTEKI       PIC 9(01).
           03   LNK-SC95-CHUSYA        PIC 9(08).
      *    ������Ź԰ٺ���̵ͭ
           03   LNK-SC95-WKSRYACT      PIC 9(01).
      *
           03  LNK-SC95-ERRCD          PIC X(04).
      *
