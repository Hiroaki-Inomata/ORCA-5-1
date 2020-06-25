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
      *****************************************************************
      *  �v���O�����C������
      * Maj/Min/Rev	�C����	���t		���e
      *  01.00.01    �g��      2007/05/17   ���[�l���ύX�ɔ����C����
      *                                     �d�l�ύX�ɔ����C��
      *  01.00.02    �g��      2007/10/05   �V�X�e���Ǘ��}�X�^�u1901�v��
      *                                     ��Ë@�֏����Q��
      *  01.00.03    ����      2018/11/07   �C�������R�����g�폜
      *****************************************************************
        01  SC1803.
            02  SC1803-HOSPCD   PIC X(20).
            02  SC1803-SRYYM    PIC X(16).
            02  SC1803-HOSPADRS PIC X(100).
            02  SC1803-HOSPNAME PIC X(100).
            02  SC1803-KOH-TBL          OCCURS  6 TIMES.
              03  SC1803-DATA-TBL         OCCURS  18 TIMES.
                04  SC1803-KENSU    PIC X(5).
                04  SC1803-ITBFTN   PIC X(7).