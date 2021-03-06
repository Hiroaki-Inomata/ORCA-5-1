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
      ************************************************************
      *    ���������ѥѥ�᥿��  COPY  (CPLNKPARA.INC)  *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  LNK-PARA-AREA.
           03  LNKPAR-KEY.
      *ü���ɣ�
               05  LNKPAR-TANMATID       PIC X(15).
      *
           03  LNKPAR-KYOUTU.
      *���ŵ��إ�����
               05  LNKPAR-HOSPID          PIC X(24).
      *�����ƥ����ա������
               05  LNKPAR-SYSDAY          PIC 9(08).
      *�����ƥ����ա������Խ��ѡ�
               05  LNKPAR-SYSDAYH         PIC X(09).
      *���ץ������
               05  LNKPAR-MOTOPG          PIC X(08).
      *��ץ������
               05  LNKPAR-SAKIPG          PIC X(08).
      *�����ֹ�
               05  LNKPAR-PTNUM           PIC X(20).
      *���ԣɣ�
               05  LNKPAR-PTID            PIC 9(10).
      *������
               05  LNKPAR-SRYDAY          PIC X(08).
      *�������������Խ��ѡ�
               05  LNKPAR-SRYDAYW         PIC X(09).
      *������ʬ    
               05  LNKPAR-NYUGAI          PIC X(01).
      *���Ի�̾����
               05  LNKPAR-KANANAME        PIC X(50).
      *���Ի�̾
               05  LNKPAR-NAME            PIC X(50).
      *��������
               05  LNKPAR-SEX             PIC X(02).
      *������ǯ����
               05  LNKPAR-BIRTHDAY        PIC X(08).
      *������ǯ�����������Խ��ѡ�
               05  LNKPAR-BIRTHDAYW       PIC X(09).
      *���Ų�
               05  LNKPAR-SRYKA           PIC X(02).
      *���Ų�̾
               05  LNKPAR-SRYKAMEI        PIC X(10).
      *�ݸ��ȹ��ֹ�
               05  LNKPAR-KUMINO          PIC X(04).
      *�ݸ��ȹ礻����
               05  LNKPAR-KUMIMEI         PIC X(36).
      *��ôΨ
               05  LNKPAR-RITU            PIC 9(03).
      *��ôΨ̾��
               05  LNKPAR-RITUMEI         PIC X(10).
      *���ջ���
               05  LNKPAR-UKETIME         PIC X(06).
      *���θ��ץ������
               05  LNKPAR-MOTOPG2         PIC X(08).
      *
