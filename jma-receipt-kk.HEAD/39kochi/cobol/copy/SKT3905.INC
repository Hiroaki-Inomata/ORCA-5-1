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
      ********************************************************************
      *    �������ґ����\�i���m���j     COPY (SKT3905.INC)
      *
      *  �쐬���t    ��Ǝ�        �L�q
      *  08/04/09    �g��          �V�K�쐬(�������ґΉ�)
      ********************************************************************
      *  �R�s�[��C������
      * Maj/Min/Rev  �C����       ���t      ���e
      ********************************************************************
       01  SKT3905.
      *    �f�ÔN��
           03  SKT3905-SRYYM                    PIC X(16).
      *    ���v�y�[�W��
           03  SKT3905-GPAGE                    PIC X(2).
      *    �y�[�W��
           03  SKT3905-PAGE                     PIC X(2).
      *    �����N����
           03  SKT3905-SEIYMD                   PIC X(22).
      *    ��Ë@�փR�[�h
           03  SKT3905-HOSPCD                   PIC X(40).
      *    ��Ë@�֏Z��
           03  SKT3905-ADRS                     PIC X(100).
      *    ��Ë@�֖���
           03  SKT3905-HOSPNAME                 PIC X(100).
      *    �J�ݎҎ���
           03  SKT3905-KAISETUNAME              PIC X(40).
      *    �d�b�ԍ�
           03  SKT3905-TEL                      PIC X(15).
      *    �����e�[�u��
           03  SKT3905-SEIKYU-TBL      OCCURS  14.
      *    ���t����
               05  SKT3905-KYURATE              PIC X(02).
      *    �ی��Ҕԍ�
               05  SKT3905-HKNJANUM             PIC X(12).
      *    ���O�e�[�u��
               05  SKT3905-NYUGAI-TBL      OCCURS  2.
      *    ����
                   07  SKT3905-KENSU            PIC X(05).
      *    �_��
                   07  SKT3905-TENSU            PIC X(09).
      *    �ꕔ���S��
                   07  SKT3905-ITBFTN           PIC X(09).
      *    �����e�[�u��
                   07  SKT3905-GENBK-TBL.
      *    ����
                       09  SKT3905-GENMARU      PIC X(02).
                       09  SKT3905-GENBK1       PIC X(12).
                       09  SKT3905-GENBK2       PIC X(03).
      *    �H�����z
               05  SKT3905-S-KINGK              PIC X(09).
      *    �W�����S�z
               05  SKT3905-S-FTNGK              PIC X(09).
      *    ���v�e�[�u��
           03  SKT3905-GOKEI-TBL       OCCURS  2.
      *    ���v����
               05  SKT3905-GKENSU               PIC X(05).
      *    ���v�_��
               05  SKT3905-GTENSU               PIC X(09).