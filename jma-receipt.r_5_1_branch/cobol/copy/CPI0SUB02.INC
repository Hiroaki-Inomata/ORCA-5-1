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
       01  I0SUB02-LNK.
      *  ��������:0/��������:1
         03  I0SUB02-SYORIFLG                      PIC 9(01).
         03  I0SUB02-RRKNUM                        PIC 9(03).
         03  I0SUB02-NISSU                         PIC 9(05).
         03  I0SUB02-SKYSTYMD                      PIC X(08).
         03  I0SUB02-SKYEDYMD                      PIC X(08).
      *  ��Ǽ�ǡ���
         03  I0SUB02-SYUNOU-TBL.
           05  I0SUB02-SYUNOU-CNT                  PIC 9(05).
           05  I0SUB02-SYUNOU-OCC                  OCCURS  23.
      *      ��Ǽ��ץե饰  0:��Ǽ�ǡ���
      *                      1:���̹�ץǡ���
      *                      2:��ץǡ���
             07  I0SUB02-SYUNOU-TTLDAT-FLG         PIC 9(01).
      *      ��Ǽ�����ե饰(��Ǽ��ץե饰=0�Τ�Τ������
      *                      0:��������
      *                      1:�ɲù���
      *                      2:�����ù���
             07  I0SUB02-SYUNOU-UPDFLG             PIC 9(01).
             07  I0SUB02-SYUNOU-YM-S               PIC X(06).
             07  I0SUB02-SYUNOU-YM-W               PIC X(06).
             07  I0SUB02-SYUNOU-HKNCOMBI           PIC X(50).
      *      �����ݸ��ȹ礻���ʷ��̹�ץǡ����Τ������
             07  I0SUB02-SYUNOU-HKNCOMBI-SU        PIC 9(02).
      *      �оݷ���ʹ�ץǡ����Τ������
             07  I0SUB02-SYUNOU-TUKI-SU            PIC 9(02).
             07  I0SUB02-SYUNOU-DAT                PIC X(3000).
             07  I0SUB02-SYUDAY-DAT                PIC X(4000).
             07  I0SUB02-UPD-SKYMONEY              PIC S9(08).
             07  I0SUB02-UPD-TAX-SAI               PIC S9(08).
         03  I0SUB02-RC                            PIC S9(02).
         03  I0SUB02-S02-RC                        PIC S9(05).