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
      *    ���莾�a�×{��̏ؕێ��҂�
      *    ���ȕ��S�������z�\����     �i�������j   COPY (SC4023.INC)
      *
      *  �쐬���t    ��Ǝ�        �L�q
      *  09/02/12    �g��          �V�K�쐬
      ********************************************************************
      *  �R�s�[��C������
      * Maj/Min/Rev  �C����  ���t        ���e
      *  01.00.01    �g��    2009/03/05  �������`�l�ǉ��ׂ̈�
      *                                  �J�ݎҖ��o�C�g������
      *  01.00.02    �g��    2009/08/20  2��ڂ̓��@���Ԃ�ǉ�
      ********************************************************************
       01  SC23.
      *    �������t
           03  SC23-SEIYMD               PIC X(22).
      *    �s������
           03  SC23-CITYNAME             PIC X(30).
      *    �s�����ԍ��i����S�Ҕԍ��j
           03  SC23-CITYNUM              PIC X(16).
      *    �f�ÔN��
           03  SC23-SRYYM                PIC X(16).
      *    ��Ë@�փR�[�h
           03  SC23-HOSPCDN              PIC X(20).
      *    ��Ë@�֏��ݒn
           03  SC23-ADRS                 PIC X(100).
      *    ��Ë@�֖�
           03  SC23-HOSPNAME             PIC X(100).
      *    �J�ݎҖ�
           03  SC23-KAISETU              PIC X(100).
      *    ��s��
           03  SC23-GINKOU               PIC X(20).
      *    �x�X��
           03  SC23-SITEN                PIC X(20).
      *    �����敪
           03  SC23-KOUZAKBN             PIC X(02)  OCCURS 2.
      *    �����ԍ�
           03  SC23-KOUZANUM             PIC X(10).
      *    ���v�����z
           03  SC23-GSEIKYUGK            PIC X(10).
      *    �����G���A
            03  SC23-SEIKYU-AREA  OCCURS 4.
      *    �ی��Ҕԍ�
                05  SC23-HKNJANUM        PIC X(16).
      *    �L���E�ԍ�
                05  SC23-KIGONUM         PIC X(36).
      *    �󋋎Ҕԍ�
                05  SC23-JKYSNUM         PIC X(14).
      *    ����
                05  SC23-NAME            PIC X(20).
      *    ���@����
                05  SC23-NYUINKIKAN      PIC X(14)  OCCURS 2.
      *    ���O�e�[�u��
                05  SC23-NYUGAI-TBL  OCCURS 2.
      *    ����
                    07  SC23-NISSU       PIC X(02).
      *    ���t�O���z
                    07  SC23-KYUFUGAIGK  PIC X(09).
      *    �������z
                    07  SC23-SEIKYUGK    PIC X(09).