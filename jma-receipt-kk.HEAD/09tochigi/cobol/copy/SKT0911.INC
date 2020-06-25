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
      *  �R�s�[��C������
      * Maj/Min/Rev  �C����  ���t        ���e
      *  01.00.01    �g��    2010/02/03  ���ʗ×{��ۑΉ�
      *  01.00.02    ���    2018/10/12  �C�������R�����g�폜
      ********************************************************************
        01  SKT0911.
      *�ی��Ҕԍ�
            02  SKT0911-HKNJANUM    PIC X(12).
      *���ԍ�
            02  SKT0911-KENNUM  PIC X(10).
      *��Ë@�փR�[�h
            02  SKT0911-HOSPCD  PIC X(20).
      *�y�[�W��
            02  SKT0911-NO  PIC X(10).
      *���ʗ×{��
            02  SKT0911-TOKUBETU        PIC X(10).
      *������ꗗ
            02  SKT0911-KOH-TBL         OCCURS  4 TIMES.
      *�×{�̋��t�F���@�E�O��
              03  SKT0911-KOH-G           OCCURS  2 TIMES.
      *�×{�̋��t�F����
                04  SKT0911-KOH-KENSU   PIC X(6).
      *�×{�̋��t�F����
                04  SKT0911-KOH-NISSU   PIC X(6).
      *�×{�̋��t�F�_��
                04  SKT0911-KOH-TENSU   PIC X(13).
      *�×{�̋��t�F�ꕔ���S��
                04  SKT0911-KOH-ITBFTN  PIC X(13).
      *****
      *�H���×{�F
              03  SKT0911-KOH-SYOG.
      *�H���×{�F����
                04  SKT0911-KOH-SYOKENSU    PIC X(6).
      *�H���×{�F����
                04  SKT0911-KOH-SYONISSU    PIC X(6).
      *�H���×{�F���z
                04  SKT0911-KOH-SYOKINGAK   PIC X(13).
      *�H���×{�F�W�����S��
                04  SKT0911-KOH-SYOFTNGAK   PIC X(13).
      *����ԍ�
              03  SKT0911-KOH-NUM PIC X(3).
      *�����
              03  SKT0911-KOH-NAME    PIC X(20).