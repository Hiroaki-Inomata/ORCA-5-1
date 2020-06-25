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
      *  01.00.01    �g��      2007/04/23   �ی���Ë@�փR�[�h��������
      *  01.00.02    �g��      2007/05/18   ���[�l���ύX�ɔ����C���ƁA
      *                                     �d�l�ύX�ɔ����C��
      *  01.00.03    �g��      2007/10/05   �V�X�e���Ǘ��}�X�^�u1901�v��
      *                                     ��Ë@�֏����Q��
      *  01.00.04    ����      2018/11/07   �C�������R�����g�폜
      *****************************************************************
        01  SC1805.
            02  SC1805-HOSPCD    PIC X(20).
            02  SC1805-SKYYM    PIC X(14).
            02  SC1805-POST PIC X(9).
            02  SC1805-HOSPADRS PIC X(100).
            02  SC1805-HOSPNAME PIC X(100).
            02  SC1805-HOSPTEL  PIC X(32).
            02  SC1805-HOSPFAX  PIC X(30).
            02  SC1805-CITYNAME PIC X(10).
            02  SC1805-CITYCD   PIC X(2).
            02  SC1805-PKENSU   PIC X(3).
            02  SC1805-PITFTN   PIC X(7).
            02  SC1805-PKOHFTN  PIC X(7).
            02  SC1805-PSHOKFTN PIC X(7).
            02  SC1805-PKOHSHOKFTN  PIC X(7).
            02  SC1805-PAGE PIC X(3).
            02  SC1805-TBL-NAIYO        OCCURS  5 TIMES.
              03  SC1805-SRYM PIC X(2).
              03  SC1805-JKYSNUM  PIC X(18).
              03  SC1805-PTNAME   PIC X(60).
              03  SC1805-GENGO
                      PIC X(2)    OCCURS  4 TIMES.
              03  SC1805-TANJOYMD.
                04  SC1805-TANJOYY  PIC X(4).
                04  SC1805-TANJOMM  PIC X(4).
                04  SC1805-TANJODD  PIC X(4).
              03  SC1805-HKNJANUM PIC X(8).
              03  SC1805-HKNKIGO  PIC X(20).
              03  SC1805-HKNBANGO PIC X(40).
              03  SC1805-HONKZKKBN    PIC X(1).
              03  SC1805-SRYKA    PIC X(2).
              03  SC1805-CHOKI    PIC X(1).
              03  SC1805-KYUFURATE    PIC X(2).
              03  SC1805-SRYONISSU    PIC X(2).
              03  SC1805-SHOKUKAISU   PIC X(2).
              03  SC1805-TENSU    PIC X(7).
              03  SC1805-SHOKUMONEY   PIC X(7).
              03  SC1805-ITBFTNMONEY  PIC X(7).
              03  SC1805-SHOKFTNMONEY PIC X(7).
              03  SC1805-TBL-KOH          OCCURS  2 TIMES.
                04  SC1805-KOHHOBETU    PIC X(2).
                04  SC1805-KOHNISSU PIC X(2).
                04  SC1805-KOHTEN   PIC X(7).
                04  SC1805-KOHFTNMONEY  PIC X(7).
                04  SC1805-KOHSHOKKAISU PIC X(2).
                04  SC1805-KOHSHOKUMONEY    PIC X(7).
                04  SC1805-KOHSHOKUFTNMONEY PIC X(7).