      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will beuseful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditionsdescribed
      * in the JMA Open Source License. You should have received acopy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku,Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      ************************************************************
      *          ������������ʣ�̼��쥻�� COPY (CPRMF10.INC) *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  MF102-REC.
           03  MF102-KEY.
      *�����ֹ�
               05  MF102-PTNUM           PIC  X(20).
      *�ݸ����ֹ�
               05  MF102-HKNJANUM        PIC  X(08).
      *Ŭ�ѳ���ǯ����
               05  MF102-TEKSTYMD        PIC  X(08).
      *�쥻�ץȼ���
               05  MF102-RECESYUBETU     PIC  X(04).
      *Ģɼ�ɣ�
           03  MF102-FORMID              PIC  X(20).
      *Ģɼ�ǡ���
           03  MF102-PRTDATA             PIC  X(10000).
