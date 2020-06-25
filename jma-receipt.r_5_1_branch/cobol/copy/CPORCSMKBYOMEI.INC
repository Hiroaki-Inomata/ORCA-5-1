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
      *    ��̾��Ω����  COPY (CPORCSMKBYOMEI.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSMKBYOMEIAREA.
           03  SMKBYO-IN.
      *        ������̾�ե饰
               05  SMKBYO-UTAGAIFLG      PIC X(01).
      *        ��­������
               05  SMKBYO-HOSOKU-COMMENT PIC X(80).
           03  SMKBYO-IO.
      *        ��̾
               05  SMKBYO-BYOMEI         PIC X(240).
      *        ��̾������
               05  SMKBYO-BYOMEICD-G.
                   07  SMKBYO-BYOMEICD   PIC X(07)
                                         OCCURS   21.
      *        �Խ���̾�ե饰
               05  SMKBYO-BYOMEIHENFLG   PIC X(01).
      *
           03  SMKBYO-OT.
      *        Ĺ���ʥХ��ȿ����ֵѡ�
               05  SMKBYO-LENGTH         PIC 9(05).
      *        �ֵѥ��ơ������ݹ�������
      *        ��0:�ѹ��ʤ���1:�ѹ������
               05  SMKBYO-STS-MODIFY     PIC 9(01).
      *        �ֵѥ��ơ������ݵ���
      *        ��0:�ѹ��ʤ���1:"�ε���"�ղá�
               05  SMKBYO-STS-UTAGAI     PIC 9(01).
      *        �ֵѥ��ơ������ݵ���
      *        ��0:�ѹ��ʤ���1:"����"�ղá�
               05  SMKBYO-STS-KYUSEI     PIC 9(01).