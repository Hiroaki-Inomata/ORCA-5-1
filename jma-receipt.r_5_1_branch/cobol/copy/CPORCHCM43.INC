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
      *    ������������ѥ�᥿      COPY (CPORCHCM43.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL������   04/03/14  ��������ۤ��ɲ�
      ************************************************************
      *
       01  ORCHCM43AREA.
      *
           03  ORCHCM43-IN.
      *
      *        �ƽи���ʬ��1:���׶�̳/2:��Ǽ��̳��
               05  ORCHCM43-KBN            PIC X(01).
      *        ���Ͽ���ǯ��
               05  ORCHCM43-STSRYYM        PIC X(06).
      *        ��λ����ǯ��
               05  ORCHCM43-EDSRYYM        PIC X(06).
      *        ȯ����ˡ
               05  ORCHCM43-HAKUHOUFLG     PIC X(01).
      *        ȯ����
               05  ORCHCM43-HAKYMD         PIC X(08).
      *
      *        �����������ȯ�ԥե饰
               05  ORCHCM43-ZEROSKYFLG     PIC X(01).
      *
           03  ORCHCM43-OT.
      *        �Խ��ڡ�����
               05  ORCHCM43-PAGE           PIC 9(08).
      *        �꥿���󥳡��ɡʣ���������ʳ����۾��
               05  ORCHCM43-RC             PIC 9(02).