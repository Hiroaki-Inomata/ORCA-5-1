      ******************************************************************
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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ******************************************************************
      *  �V�X�e����        : �n�q�b�`
      *  �T�u�V�X�e����    : �������[
      *  �R���|�[�l���g��  : �������ꗗ�\
      *  �Ǘ���            : 
      *  Maj/Min/Rev  �C����  ���t       ���e
      *   01.00.00    ���    11/03/29   �V�K�쐬
      ******************************************************************
       01  TA03TMP-REC.
           03  TA03TMP-KEY.
      *      ���Ҕԍ�
             05  TA03TMP-PTNUM       PIC X(20).
      *      �`�[�ԍ�
             05  TA03TMP-DENPNUM     PIC X(7).
      *    ����
           03  TA03TMP-NAME          PIC X(20).
      *    �ی�
           03  TA03TMP-HKN           PIC X(4).
      *    ��������
           03  TA03TMP-SEIKYUMD      PIC X(9).
      *    �a���^�f�É�
           03  TA03TMP-SRYKA-BR      PIC X(8).
      *    �ی��v
           03  TA03TMP-HKN-TOTAL     PIC 9(9).
      *    ���ҕ��S
           03  TA03TMP-KANJAFTN      PIC 9(9).
      *    ����
           03  TA03TMP-JIHI          PIC 9(9).
      *    �����z
           03  TA03TMP-MISYU         PIC S9(9).
      *    �����z
           03  TA03TMP-NYUKIN        PIC 9(9).
      *    ��������
           03  TA03TMP-NYUKINMD      PIC X(9).
      *    �ŏI��f��
           03  TA03TMP-JYUSHINYMD    PIC X(9).
      *    �d�b�ԍ�
           03  TA03TMP-TEL           PIC X(15).