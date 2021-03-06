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
      *     ����ƣ����ͼ����ϥѥ�᥿    COPY (CPORCHC33.INC)
      *                                          (ORCHC33 �ǻ��ѡ�
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     NACL-¿��Ǽ  04/12/1X  ������ʬ���ɲ�
      ************************************************************
      *
       01  ORCHC33AREA.
      *
      *    ������ʬ�ʣ�������饤�󡢣�������
           03  ORCHC33-KBN             PIC X(01).
      *
      *    �����ֹ�
           03  ORCHC33-PTNUM           PIC X(20).
      *    ���ԣɣ�
           03  ORCHC33-PTID            PIC 9(10).
      *    ���Ի�̾
           03  ORCHC33-NAME            PIC X(50).
      *    ��������
           03  ORCHC33-SEX             PIC X(02).
      *    ������ǯ����
           03  ORCHC33-BIRTHDAY        PIC X(08).
      *    ����ǯ��
           03  ORCHC33-SRYYM           PIC X(06).
      *    ����ǯ��ʽ�λ��
           03  ORCHC33-SRYYM-END       PIC X(06).
      *    ������ʬ
           03  ORCHC33-NYUGAIKBN       PIC X(01).
      *    ���Ų�
           03  ORCHC33-SRYKA           PIC X(02).
      *    �ݸ��ȹ礻
           03  ORCHC33-HKNCOMBI        PIC X(04).
      *
      *    ������ʬ��1:Ƭ��0:����)
           03  ORCHC33-PRINT-FLG       PIC 9(01).
      *    ����������
           03  ORCHC33-STRDD           PIC 9(02).
      *    �ǽ�������
           03  ORCHC33-ENDPAGE         PIC 9(03).
      *    �ǽ�������
           03  ORCHC33-ENDLINE         PIC 9(02).
      *    ��������
           03  ORCHC33-KARUTE3KBN      PIC X(01).
      *
      *    ���ѥ�᥿
           03  ORCHC33-PRTAREA         PIC X(25000).
      *
      *�����ΰ�
      *    �ڡ������
           03  ORCHC33-PAGE            PIC 9(04).
      *
      *    ���顼������
           03  ORCHC33-RC              PIC 9(02).
      *
