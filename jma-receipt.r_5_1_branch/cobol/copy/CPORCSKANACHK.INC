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
      *    ���ѹ��ܥ����å��ѥ�᥿��  COPY (CPORCSKANACHK.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     NACL-¿��Ǽ  02/08/09  ���ϡ������ΰ��X(1000)��
      * 04.05.00     NACL-¿��Ǽ  09/12/11  ���Ѷ������ɲ�
      * 04.06.00     NACL-¿��Ǽ  10/10/01  ���ϡ������ΰ��X(5000)��
      * 05.01.00     NACL-¿��Ǽ  17/09/26  ���������ɣ�(���ԡ��ɲ�
      ************************************************************
      *
       01  ORCSKANACHKAREA.
      *    �����ե饰�ʣ��������å������������Ѵ���
           03  KANACHK-SYORI               PIC X(01).
      *    �����ե饰���ʣ�����������Ѷ�������
           03  KANACHK-SYORIKBN            PIC X(01).
      *    �����ΰ�
           03  KANACHK-MAE-INPUT           PIC X(5000).
      *    ���Ϸ�������Ѥκ������λ���ʾ�ά�ġˡ�
           03  KANACHK-MAX-CNT             PIC 9(04).
      *
      *    �����ΰ��Ⱦ�Ѷ�������Ѥ��ִ������Խ���
           03  KANACHK-OUT-INPUT           PIC X(5000).
      *    ʸ�������סʣ���Ⱦ�ѡ��������ѡ�
           03  KANACHK-SYUBETU             PIC 9(01).
      *    ������
           03  KANACHK-MAX                 PIC 9(04).
      *    ���������� �ʣ�������������ߥ��顼�������Ѵ����顼��
           03  KANACHK-RC                  PIC 9(04).
      *    ���������ɣ��ʣ����ѿ����ʳ��ʤ��������ѿ����ʳ����ꡢ
      *                  �������򤢤��
           03  KANACHK-RC2                 PIC 9(04).
      *    ���������ɣ��ʣ������Ѷ���ʤ����������Ѷ��򤢤�
      *                  �������Ѷ���Τߡ������Ǹ夬���Ѷ����
           03  KANACHK-RC3                 PIC 9(01).
      *
      *    ���������ɣ��ʣ������Ԥʤ����������ԥ����ɤ����
           03  KANACHK-RC4                 PIC 9(01).
      *
      *