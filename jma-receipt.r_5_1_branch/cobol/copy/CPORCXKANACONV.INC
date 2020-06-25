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
      *    ʸ��������å��ؿ��ѥ�᥿��  COPY (CPORCSKANACONV.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     
      ************************************************************
      *
       01  ORCXKANACONVAREA.
      *   ���ϥХ��ȿ�
           03 KANACONV-LEN             PIC S9(9) BINARY.
      *   �֤���( 0: ���� 1: �ϰϳ�ʸ�� 2: �������Ѵ���ǽ )
           03 KANACONV-RETURN          PIC S9(9) BINARY.
      *   �Ѵ�ʸ�����(�Х���)
           03 KANACONV-MAXLEN          PIC S9(9) BINARY.
      *   �Ѵ��ե饰 (0: �Ѵ����ʤ�,     1: �Ѵ�����
      *               2: 1+���������Ѵ�, 3: 1+�Ҥ餬���Ѵ�)
      *   �� Ⱦ�ѥ��ʤ���ӳ��������ƤΥե饰���Ѵ������
      *      ���ΤƤ��롣
      *     (�����Ϣ�)
           03 KANACONV-CONV-FLAG       PIC S9(9) BINARY.
      *   ����ʸ�������
      *       0: Ⱦ�ѥ��ʡ������Τ��Ѵ�
      *      14: ����   2: �Ҥ餬��  4: ��������
      *      112: ASCII 16: ����     32: ����ե��٥å�
      *      15: ����+���ڡ�������   113: ASCII+���ڡ������� 
      *   ����ʸ����� OR ��Ȥ뤳�Ȥ�ʣ������Ǥ���
      *   ��: (6: �Ҥ餬�ʡ��������ʤΤ�)
      *       (48: �ѿ����Τ�)
           03 KANACONV-CHAR-TYPE       PIC S9(9) BINARY.
      *   ʸ���ΰ��Ĺ��(INDATA, OUTDATA��Ĺ����Ʊ���Ǥ��뤳��)
           03 KANACONV-STLEN           PIC S9(9) BINARY.
      *   ����ʸ����
           03 KANACONV-INDATA          PIC X(1000).
      *   ����ʸ����
           03 KANACONV-OUTDATA         PIC X(1000).
      *
      *