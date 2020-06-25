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
      *    �������Ź԰ٹ����ѥ�᥿����COPY (CPORCSCGAIRAI.INC)
      *                           ���ѥץ������ (ORCGK08,ORCGK03)
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSCGAIRAIAREA.
      *
      *��ʬ�ʣ������Ź԰ٺ������������Ź԰ٺ����������ǻ������Τߡ�
           03  ORCSCGAIRAI-KBN                 PIC  X(01).
      *�ˣ����Τ�
           03  ORCSCGAIRAI-K03-AREA.
      *    Ĵ����
               05  ORCSCGAIRAI-CHOSEI          PIC S9(07).
      *    ����������ʬ
               05  ORCSCGAIRAI-HAKFLG          PIC X(01).
      *    ���������
               05  ORCSCGAIRAI-KON-SKYMONEY    PIC S9(07).
      *    �����
               05  ORCSCGAIRAI-NYUKIN          PIC S9(07).
      *    ������ˡ
               05  ORCSCGAIRAI-NYUKIN-HOHO     PIC X(02).
      *   �ʣ����ޤȤ��³�������ޤȤ�����
               05  ORCSCGAIRAI-SYORI           PIC 9(01).
      *    ȯ����ˡ
               05  ORCSCGAIRAI-HAKHOUFLG       PIC 9(01).
      *    ȯ����
               05  ORCSCGAIRAI-HAKYMD          PIC X(08).
      *
      *    ��ɼ�ֹ�
      *!       05  ORCSCGAIRAI-DENPNUM         PIC 9(07).
      *    ���������ֹ�
      *!       05  ORCSCGAIRAI-JYURRK-RENNUM   PIC 9(01).
      *    ��Ǽ�ΰ�
               05  ORCSCGAIRAI-SYUNOU-AREA.
                   07  ORCSCGAIRAI-SYUNOU-MAX  PIC 9(04).
                   07  ORCSCGAIRAI-SYUNOU-TBL  OCCURS  15.
                       09  ORCSCGAIRAI-SYUNOU-REC  PIC X(2000).
      *                ���������ֹ�
                       09  ORCSCGAIRAI-JYURRK-RENNUM   PIC 9(01).
      *H18/9
      *                �����ֶ��
                       09  ORCSCGAIRAI-SYU-HENKIN      PIC S9(07).
                       09  ORCSCGAIRAI-SYU-HENKBN      PIC  X(01).
      *