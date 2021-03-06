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
      *    ����ϫ���ݸ�����    �� COPY  (CPPTRSIINF.INC)  *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.07.00    NACL-¿��Ǽ  14/10/XX  �軰�Ը�ʪ�ٵ��о����ɲ�
      ************************************************************
      *01  PTRSIINF-REC.
      *����
           03  PTRSI-KEY.
      *���ŵ��أɣ�
               05  PTRSI-HOSPNUM            PIC 9(2).
      *���ԣɣ�
               05  PTRSI-PTID              PIC  9(10).
      *�ݸ��ɣ�
               05  PTRSI-HKNID             PIC  9(10).
      *����ɣ�
               05  PTRSI-KOHID             PIC  9(10).
      *ϫ���ݸ���ʬ
           03  PTRSI-HKNKBN                PIC  X(01).
      *�ݸ��ֹ�
           03  PTRSI-HKNNUM                PIC  X(03).
      *���Ը����ֹ�
           03  PTRSI-KOFUNUM               PIC  X(14).
      *���ݸ���̾
           03  PTRSI-HIHKNJANAME           PIC  X(100).
      *���Ƚ�̾��
           03  PTRSI-JIGYOUNAME            PIC  X(80).
      *�������ƻ�ܸ�̾
           03  PTRSI-PREFNAME              PIC  X(20).
      *�������ƻ�ܸ���ʬ
           03  PTRSI-PREFKBN               PIC  X(01).
      *����Ϸ��Զ�̾
           03  PTRSI-CITYNAME              PIC  X(80).
      *����Ϸ��Զ��ʬ
           03  PTRSI-CITYKBN               PIC  X(01).
      *���Ѻ���
           03  PTRSI-SINKEI                PIC  X(01).
      *ž����ͳ
           03  PTRSI-TENKI                 PIC  X(01).
      *����ǯ����
           03  PTRSI-SHOBYOYMD             PIC  X(08).
      *�ͻ������ʬ
           03  PTRSI-SISIKBN               PIC  X(01).
      *���ܳ���ǯ����
           03  PTRSI-RYOSTYMD              PIC  X(08).
      *���ܽ�λǯ����
           03  PTRSI-RYOEDYMD              PIC  X(08).
      *»����ʬ
           03  PTRSI-SONSHOUKBN            PIC  X(02).
      *»����ʬ�ݻ���
           03  PTRSI-SONSHOUKBN-EDANUM     PIC  X(01).
      *���ե������ܳ���ǯ����
           03  PTRSI-SINSATUYMD            PIC  X(08).
      *���ե������ܽ�λǯ����
           03  PTRSI-SINSATUEND            PIC  X(08).
      *��ǧǯ����
           03  PTRSI-KAKUNINYMD            PIC  X(08).
      *�����ʬ
           03  PTRSI-SAKUJOKBN             PIC  X(01).
      *ϫ�ҥ�����
           03  PTRSI-COMMENT               PIC  X(200).
      *�ҳ���ʬ
           03  PTRSI-SAIGAIKBN             PIC  X(01).
      *�����մ��������ʬ
           03  PTRSI-JIBAISEIKBN           PIC  X(01).
      *������ô���女����
           03  PTRSI-DRCD                  PIC  X(05).
      *�������ݸ����̾
           03  PTRSI-HOKENNAME             PIC  X(80).
      *ϫ�ҥ쥻������ܡ����ǯ��
           03  PTRSI-KIJYUNYM              PIC  X(06).
      *ϫ�ҥ쥻������ܡ����
           03  PTRSI-KAISU                 PIC  9(03).
      *ϫƯ�����Ľ𥳡���
           03  PTRSI-SYOCD                 PIC  X(05).
      *�軰�ԡ���ʪ���շ׻���ʬ
           03  PTRSI-DAISAN-GENBTKBN       PIC  X(01).
      *�軰�ԡ��õ������ʬ
           03  PTRSI-DAISAN-TOKKIKBN       PIC  X(01).
      *�����ն�ʬ
           03  PTRSI-JIBAIKBN              PIC  X(01).
      *ü���ɣ�
           03  PTRSI-TERMID                PIC  X(16).
      *���ڡ��졼���ɣ�
           03  PTRSI-OPID                  PIC  X(16).
      *����ǯ����
           03  PTRSI-CREYMD                PIC  X(08).
      *����ǯ����
           03  PTRSI-UPYMD                 PIC  X(08).
      *��������
           03  PTRSI-UPHMS                 PIC  X(06).
