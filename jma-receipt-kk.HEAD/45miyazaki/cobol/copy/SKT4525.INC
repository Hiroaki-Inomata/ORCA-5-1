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
      *  �������Ґ������i�{�茧�j     COPY (SKT4525.INC)               
      *
      *  �쐬���t    ��Ǝ�        �L�q
      *  08/05/01    ��|          �V�K�쐬(�������ґΉ�)
      ********************************************************************
      *  �R�s�[��C������
      * Maj/Min/Rev  �C����       ���t      ���e
      ********************************************************************
        01  SKT4525.
      *    �ی��Ҕԍ�
            03  SKT4525-HKNJANUM          PIC X(12).
      *    ��Ë@�փR�[�h
            03  SKT4525-HOSPCD            PIC X(14).
      *    �y�[�W�ԍ�
            03  SKT4525-NO                PIC X(04).
      *    �f�ÔN��
            03  SKT4525-SRYYM             PIC X(16).
      *    �����N����
            03  SKT4525-SEIYMD            PIC X(22).
      *    �Z�� 
            03  SKT4525-ADRS              PIC X(100).
      *    ��Ë@�֖�
            03  SKT4525-HOSPNAME          PIC X(100).
      *    �J�ݎҖ�
            03  SKT4525-KAISETUNAME       PIC X(40).
      *    �d�b�ԍ�
            03  SKT4525-TEL               PIC X(15).
      *    �ݑ�
            03  SKT4525-ZAI1              PIC X(02).
            03  SKT4525-ZAI2              PIC X(02).
      *    �����e�[�u��
            03  SKT4525-HKN-TBL.
              05  SKT4525-SEIKYU-TBL  OCCURS  2 TIMES.
                07  SKT4525-NYUGAI-TBL  OCCURS  2 TIMES.
                  09  SKT4525-KENSU       PIC X(05).
                  09  SKT4525-NISSU       PIC X(05).
                  09  SKT4525-TENSU       PIC X(09).
                  09  SKT4525-ITBFTN      PIC X(09).
                07  SKT4525-S-KENSU       PIC X(05).
                07  SKT4525-S-NISSU       PIC X(05).
                07  SKT4525-S-KINGK       PIC X(09).
                07  SKT4525-S-FTNGK       PIC X(09).
              05  SKT4525-KOUHI-TBL  OCCURS  4 TIMES.
                07  SKT4525-HBTNUM        PIC X(02).
                07  SKT4525-KNYUGAI-TBL  OCCURS  2 TIMES.
                  09  SKT4525-KKENSU      PIC X(05).
                  09  SKT4525-KNISSU      PIC X(05).
                  09  SKT4525-KTENSU      PIC X(09).
                  09  SKT4525-KITBFTN     PIC X(09).
                07  SKT4525-S-KKENSU      PIC X(05).
                07  SKT4525-S-KNISSU      PIC X(05).
                07  SKT4525-S-KKINGK      PIC X(09).
                07  SKT4525-S-KFTNGK      PIC X(09).