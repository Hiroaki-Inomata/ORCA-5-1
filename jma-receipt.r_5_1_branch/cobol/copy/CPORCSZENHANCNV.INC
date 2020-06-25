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
      *    ���Ѣ�Ⱦ���Ѵ��ѥѥ�᥿      COPY (CPORCSZENHANCNV.INC)
      *                                  ���ѥץ������ (ORCSZENHANCNV)
      *    �������
      *      ������ʬ  ��������  ��������
      *      ����ʸ��
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *
       01  ORCSZENHANCNVAREA.
      *    ������ʬ    ��������
      *                ��������
      *                ����ʸ��
           03  LNK-ZENHANCNV-SYORI           PIC X(01).
      *    ����������
           03  LNK-ZENHANCNV-RC              PIC 9(01).
      *
      *    ���Ͼ���
           03  LNK-ZENHANCNV-IN.
      *        ����ʸ��
               05  LNK-ZENHANCNV-IN-DATA     PIC X(50).
      *        ���ϥ����å�ʸ����
               05  LNK-ZENHANCNV-IN-LEN      PIC 9(02).
      *        ����ʸ���� 
               05  LNK-ZENHANCNV-IN-LEN1     PIC 9(02).
      *
      *    ���Ͼ���
           03  LNK-ZENHANCNV-OUT.
      *        ���ա������
               05  LNK-ZENHANCNV-OT-SYMD    PIC X(08).
      *        ���ա������
               05  LNK-ZENHANCNV-OT-WYMD    PIC X(07).
      *        ���ա�ɽ����
               05  LNK-ZENHANCNV-OT-GYMD    PIC X(09).
      *        ʸ��
               05  LNK-ZENHANCNV-OT-DATA    PIC X(25).
      *        
               05  LNK-ZENHANCNV-OT-LEN     PIC 9(02).
      *        ����
               05  LNK-ZENHANCNV-OT-NUM     PIC 9(18).