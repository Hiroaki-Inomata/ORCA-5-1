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
        01  SKT0910.
      *�f�ÔN��
            02  SKT0910-PRTYM   PIC X(16).
      *�ی��Ҕԍ�
            02  SKT0910-HKNJANUM    PIC X(12).
      *�����N����
            02  SKT0910-SEIYMD  PIC X(22).
      *���ԍ�
            02  SKT0910-KENNUM  PIC X(4).
      *��Ë@�փR�[�h
            02  SKT0910-HOSPCD  PIC X(20).
      *��Ë@�֏��ݒn
            02  SKT0910-ADRS    PIC X(100).
      *��Ë@�֖���
            02  SKT0910-HOSPNAME    PIC X(100).
      *�J�ݎҎ���
            02  SKT0910-KAISETUNAME PIC X(30).
      *�y�[�W�ԍ�
            02  SKT0910-NO  PIC X(10).
      *�×{�̋��t
            02  SKT0910-HKN-TBL.
      *�ꗗ
              03  SKT0910-HKN-TBLG        OCCURS  9 TIMES.
      *���@�E�O���e�[�u��
                04  SKT0910-HKN-G           OCCURS  2 TIMES.
      *�×{�̋��t�F����
                  05  SKT0910-KENSU   PIC X(6).
      *�×{�̋��t�F����
                  05  SKT0910-NISSU   PIC X(6).
      *�×{�̋��t�F�_��
                  05  SKT0910-TENSU   PIC X(13).
      *�×{�̋��t�F�ꕔ���S��
                  05  SKT0910-ITBFTN  PIC X(13).
      *�H���×{
            02  SKT0910-SYO-TBL.
      *�ꗗ
              03  SKT0910-SYO-TBLG        OCCURS  9 TIMES.
      *�H���×{�F����
                04  SKT0910-SYOKENSU    PIC X(6).
      *�H���×{�F����
                04  SKT0910-SYONISSU    PIC X(6).
      *�H���×{�F���z
                04  SKT0910-SYOKINGAK   PIC X(13).
      *�H���×{�F�W�����S��
                04  SKT0910-SYOFTNGAK   PIC X(13).
            02  SKT0910-TITLE1  PIC X(10).
            02  SKT0910-HYOBETU
                    PIC X(2)    OCCURS  2 TIMES.
      *���ʗ×{��
            02  SKT0910-TOKUBETU        PIC X(10).