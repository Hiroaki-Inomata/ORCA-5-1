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
      *--------------------------------------------------------*
      *    �ե��������ѥ�᥿  COPY (CPORCSQRENCODE.INC)     *
      *--------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 04.08.00     NACL-¿��Ǽ  14/07/30  ����ǥ��쥯�ȥ��б�
      ************************************************************
      *
       01  ORCSQRENCODEAREA.
      *      ���ϥե�����̾
      **** 03  QREN-FILENAME           PIC X(256).
           03  QREN-FILENAME           PIC X(1024).
      *      �ѣҲ����ե�����̾
      **** 03  QREN-QRFILENAME         PIC X(256).
           03  QREN-QRFILENAME         PIC X(1024).
      *      QRENCODE -l ���ץ����(L)
           03  QREN-SL-OPTION          PIC X(01).
      *      QRENCODE -s ���ץ����(1)
           03  QREN-SS-OPTION          PIC X(01).
      *      QRENCODE -m ���ץ����(2)
           03  QREN-SM-OPTION          PIC X(01).
      *      QRENCODE -v ���ץ����(12)
           03  QREN-SV-OPTION          PIC X(02).
      *      QRENCODE -k ���ץ����(0:����ʤ�,1:-k)
           03  QREN-SK-OPTION          PIC X(01).
      *      QRENCODE -C ���ץ����(0:����ʤ�,1:-C)
           03  QREN-LC-OPTION          PIC X(01).
      *      �꥿���󥳡���(non -C:�ѣҲ����С������)
           03  QREN-RET-VERSION        PIC X(02).
      *      �꥿���󥳡���(�ѣҲ����ե������)
           03  QREN-RET-COUNT          PIC X(02).
      *      �꥿���󥳡���
           03  QREN-RET                PIC X(01).