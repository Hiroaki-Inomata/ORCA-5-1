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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      ******************************************************************
      *    ���Уɴ�����Ͽ������ݸ�������μ��ࡢ�����ʬ��(xml2)
      *                                   COPY (CPINSURANCEINF1V2RES.INC)
      ******************************************************************
       01  XML-INSURANCEINFRES.
           02  INSUINFS-CONTEXT          PIC S9(9)   BINARY.
           02  INSUINFS-OBJECT           PIC X(36).
           02  INSUINFS-MODE             PIC S9(9)   BINARY.
           02  INSUINFS-RECORDNAME       PIC X(128).
           02  INSUINFS-INSURANCEINF-RES.
      *        �»���
               03  INSUINFS-INFORMATION-DATE   PIC X(10).
      *        �»ܻ���
               03  INSUINFS-INFORMATION-TIME   PIC X(8).
      *        �ֵѥ�����
               03  INSUINFS-API-RESULT         PIC X(2).
      *        �ֵѥ�å�����
               03  INSUINFS-API-RESULT-MSG     PIC X(100).
               03  INSUINFS-RESKEY             PIC X(100).
      *
      *        �������
               03  INSUINFS-BASE-DATE           PIC X(10).
      *
      *        �ݸ�����
               03  INSUINFS-INSURANCE-INFO-G.
      *            �ݸ�����
                   05  INSUINFS-INSURANCE-TBL   OCCURS   99.
      *                �ݸ��μ���
                       07  INSUINFS-INSURANCE-CLASS    PIC X(03).
      *                �ݸ��μ���̾��
                       07  INSUINFS-INSURANCE-NAME     PIC X(20).
      *                ˡ���ֹ�
                       07  INSUINFS-INSURANCE-HKNHBTNUM    PIC X(02).
      *                �����ʬ
                       07  INSUINFS-ASSISTANCE-TBL     OCCURS  30.
      *                    �����ʬ
                           09  INSUINFS-ASSISTANCE         PIC X(01).
      *                    �����ʬ
                           09  INSUINFS-ASSISTANCE-NAME    PIC X(20).
      *                    ����Զ�ʬ�ʣ�������ԤΤߡ�
                           09  INSUINFS-ASSISTANCE-MODE    PIC X(01).
      *        �������
               03  INSUINFS-PUBLIC-INSURANCE-G.
      *            �������
                   05  INSUINFS-PUBLIC-INSURANCE-TBL  OCCURS  200.
      *                ����μ���
                       07  INSUINFS-PUBLIC-CLASS         PIC X(03).
      *                ����μ���̾��
                       07  INSUINFS-PUBLIC-CLASS-NAME     PIC X(20).
      *                �����ˡ���ֹ�
                       07  INSUINFS-PUBLIC-HKNHBTNUM      PIC X(02).
      *                ����λ�ʧ��ʬ��Ϸ�ͤΤ�
                       07  INSUINFS-PUBLIC-PAYKBN         PIC X(02).
      *
