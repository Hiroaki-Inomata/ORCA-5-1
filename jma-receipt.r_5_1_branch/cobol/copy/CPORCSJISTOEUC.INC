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
      *    ʸ�����Ѵ�(sjis->euc)�ؿ��ѥ�᥿ COPY (CPORCSJISTOEUC.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     
      ************************************************************
      *
       01  ORCSJISTOEUC.
      *   �Ѵ�ʸ�����(�Х���)
           03 SJISTOEUC-INLEN           PIC S9(9) BINARY.
      *   ���ϥХ��ȿ�
           03 SJISTOEUC-OUTLEN          PIC S9(9) BINARY.
      *   �֤���( 0: ���� 1: �ϰϳ�ʸ�� )
           03 SJISTOEUC-RETURN          PIC S9(9) BINARY.
      *   ����ʸ����
           03 SJISTOEUC-INDATA          PIC X(10000).
      *   ����ʸ����
           03 SJISTOEUC-OUTDATA         PIC X(10000).