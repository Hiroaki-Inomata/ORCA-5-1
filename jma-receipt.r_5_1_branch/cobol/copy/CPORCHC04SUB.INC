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
      *    ������������ٽ���ϥѥ�᥿       COPY (CPORCHC04.INC)
      *                                          (ORCHC04 �ǻ��ѡ�
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.07.00    NACL-¿��Ǽ  14/10/XX  �����ȥ������о����ɲ�
      ************************************************************
      *
       01  ORCHC04SUBAREA.
      *
      *    ��ɼ�ֹ��ɬ�ܡ�
           03  ORCHC04SUB-DENPNUM      PIC 9(07).
      *    ���ñ�̡ʥץ�����४�ץ�����
           03  ORCHC04SUB-GOKEITANI    PIC X(01).
      *    ���סʣ�������ˡ�������
           03  ORCHC04SUB-PARAKBN      PIC X(01).
      *    �����ȥ������оݡʣ����оݡ�
           03  ORCHC04SUB-PARACOMKBN   PIC X(01).
      *    �ݸ����оݡʣ����оݡ�
           03  ORCHC04SUB-PARAJIHIKBN  PIC X(01).
      *    API�ֵ���
           03  ORCHC04SUB-APIKBN       PIC X(01).
      *����
           03  ORCHC04SUB-INFO-AREA.
               05  ORCHC04SUB-SRYYMD           PIC X(08).
               05  ORCHC04SUB-SRYYMDMEI        PIC X(22).
               05  ORCHC04SUB-SRYKA            PIC X(02).
               05  ORCHC04SUB-SRYKAMEI         PIC X(30).
               05  ORCHC04SUB-HKNCOMBI         PIC X(04).
               05  ORCHC04SUB-HKNCOMBIMEI      PIC X(40).
      *    ��������
           03  ORCHC04SUB-LIST-AREA.
               05  ORCHC04SUB-HEN-G    OCCURS   800.
      *            �Խ��Ѥ�����
                   07  ORCHC04SUB-H-SRYKBN        PIC X(10).
                   07  ORCHC04SUB-NAME            PIC X(140).
                   07  ORCHC04SUB-TANINAME        PIC X(10).
                   07  ORCHC04SUB-H-SURYO         PIC X(11).
                   07  ORCHC04SUB-H-TENSU.
                       09  ORCHC04SUB-H-TENSU1    PIC X(02).
                       09  ORCHC04SUB-H-TENSU2    PIC X(07).
                   07  ORCHC04SUB-H-KINGAKU.
                       09  ORCHC04SUB-H-KINGAKU1  PIC X(02).
                       09  ORCHC04SUB-H-KINGAKU2  PIC X(09).
                   07  ORCHC04SUB-H-KAISU         PIC X(03).
      *            �޳���
                   07  ORCHC04SUB-H-ZAIHEN        PIC X(02).
      *            ���̡�ñ�̡����ѡ�
                   07  ORCHC04SUB-H-ZEN-SURYO     PIC X(40).
                   07  ORCHC04SUB-H-ZENMAX        PIC 9(02).
      *            �����ߡ�������
                   07  ORCHC04SUB-H-ENKBN         PIC X(01).
      *
                   07  ORCHC04SUB-SRYKBN          PIC X(02).
      *            ����
                   07  ORCHC04SUB-SURYO           PIC 9(05)V9(05).
      *            ���
                   07  ORCHC04SUB-KAISU           PIC 9(03).
      *            �����ʥ����ɤ������ˡ��о�ʬ�Τߡ�
                   07  ORCHC04SUB-TENSU           PIC 9(07).
      *            ������
                   07  ORCHC04SUB-ZAITENSU        PIC S9(08).
      *            ���ť�����
                   07  ORCHC04SUB-SRYCD           PIC X(09).
      *            �޽�λ
                   07  ORCHC04SUB-ZAIEND          PIC X(01).
      *
               05  ORCHC04SUB-TBL-MAX         PIC  9(04).
      *