      *******************************************************************
      * Project code name "ORCA"
      * ����W�����Z�v�g�\�t�g�iJMA standard receipt software�j
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
       01  SC09.
      *�敪
           02  SC09-KBN                 PIC X(2)  OCCURS 3 TIMES.
      *�f�ÔN��
           02  SC09-SRYYM               PIC X(16).
      *��Ë@�փR�[�h
           02  SC09-HOSPCD              PIC X(20).
      *��Ë@�֖���
           02  SC09-HOSPNAME            PIC X(100).
      *����
           02  SC09-KENSU               PIC X(6).
      *��o�s����
           02  SC09-TBL-CITY            OCCURS 33 TIMES.
             03  SC09-CITY                PIC X(2).
      *��o�s������
           02  SC09-CITYKENSU           PIC X(3).