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
      *    ����                           COPY (CPPTINF.INC)     *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     NACL-¿��Ǽ  05/09/13  ������ˡ�ɲ�
      ************************************************************
      *01  PTINF-REC.
      *����
           03  PTINF-KEY.
      *���ŵ��أɣ�
               05  PTINF-HOSPNUM              PIC 9(2).
      *���ԣɣ�
               05  PTINF-PTID                PIC  9(10).
      *���ʻ�̾
           03  PTINF-KANANAME                PIC  X(100).
      *������̾
           03  PTINF-NAME                    PIC  X(100).
      *�̾�
           03  PTINF-NICKNAME                PIC  X(100).
      *����
           03  PTINF-SEX                     PIC  X(01).
      *��ǯ����
           03  PTINF-BIRTHDAY                PIC  X(08).
      *��˴��ʬ
           03  PTINF-DEATHKBN                PIC  X(01).
      *�����͹���ֹ�
           03  PTINF-HOME-POST               PIC  X(07).
      *����ݽ���
           03  PTINF-HOME-ADRS               PIC  X(200).
      *�������������
           03  PTINF-HOME-BANTI              PIC  X(200).
      *������������ֹ�
           03  PTINF-HOME-TEL1               PIC  X(015).
      *������������ֹ�
           03  PTINF-HOME-TEL2               PIC  X(015).
      *���Ӽ�̾
           03  PTINF-SETAINUSI               PIC  X(100).
      *³��
           03  PTINF-ZOKUGARA                PIC  X(060).
      *����
           03  PTINF-JOB                     PIC  X(040).
      *���������ֹ�
           03  PTINF-KEITAI-TEL              PIC  X(015).
      *�ƣ����ֹ�
           03  PTINF-FAX                     PIC  X(015).
      *�Żҥ᡼�륢�ɥ쥹
           03  PTINF-EMAIL                   PIC  X(050).
      *�±����Ȼ�ͳ��ʬ
           03  PTINF-DISCOUNT-KBN            PIC  X(02).
      *���Ψ
           03  PTINF-DISCOUNT                PIC  X(02).
      *���ֶ�ʬ��
           03  PTINF-CONDITION-1             PIC  X(02).
      *���ֶ�ʬ��
           03  PTINF-CONDITION-2             PIC  X(02).
      *���ֶ�ʬ��
           03  PTINF-CONDITION-3             PIC  X(02).
      *�����ȣ�
           03  PTINF-COMMENT-1               PIC  X(200).
      *�����ȣ�
           03  PTINF-COMMENT-2               PIC  X(200).
      *�ش���
           03  PTINF-TABOO-1                 PIC  X(200).
      *�ش���
           03  PTINF-TABOO-2                 PIC  X(200).
      *����륮����
           03  PTINF-ALLERGY-1               PIC  X(200).
      *����륮����
           03  PTINF-ALLERGY-2               PIC  X(200).
      *�����ɣ�
           03  PTINF-KANSENSYO-1             PIC  X(200).
      *�����ɣ�
           03  PTINF-KANSENSYO-2             PIC  X(200).
      *Ϣ�����̾��
           03  PTINF-RENRAKU-NAME            PIC  X(100).
      *Ϣ�����³��
           03  PTINF-RENRAKU-ZOKUGARA        PIC  X(60).
      *Ϣ�����͹���ֹ�
           03  PTINF-RENRAKU-POST            PIC  X(07).
      *Ϣ����ݽ���
           03  PTINF-RENRAKU-ADRS            PIC  X(200).
      *Ϣ�������������
           03  PTINF-RENRAKU-BANTI           PIC  X(200).
      *Ϣ������������ֹ�
           03  PTINF-RENRAKU-TEL1            PIC  X(015).
      *Ϣ������������ֹ�
           03  PTINF-RENRAKU-TEL2            PIC  X(015).
      *��̳���̾��
           03  PTINF-OFFICE-NAME             PIC  X(100).
      *��̳���͹���ֹ�
           03  PTINF-OFFICE-POST             PIC  X(07).
      *��̳��ݽ���
           03  PTINF-OFFICE-ADRS             PIC  X(200).
      *��̳�����������
           03  PTINF-OFFICE-BANTI            PIC  X(200).
      *��̳��������ֹ�
           03  PTINF-OFFICE-TEL              PIC  X(015).
      *�������̾��
           03  PTINF-KISEI-NAME              PIC  X(100).
      *�������͹���ֹ�
           03  PTINF-KISEI-POST              PIC  X(07).
      *������ݽ���
           03  PTINF-KISEI-ADRS              PIC  X(200).
      *���������������
           03  PTINF-KISEI-BANTI             PIC  X(200).
      *������������ֹ�
           03  PTINF-KISEI-TEL               PIC  X(015).
      *�ƥ��ȴ����ֹ��ʬ
           03  PTINF-TSTPTNUMKBN             PIC  X(01).
      *������ˡ
           03  PTINF-NYUKIN-HOHO             PIC  X(02).
      *������ֹ�
           03  PTINF-MAXZAINUM               PIC  9(08).
      *���������ֹ�
           03  PTINF-RRKNUM                  PIC  9(03).
      *����������ֹ�
           03  PTINF-RRKEDANUM               PIC  9(03).
      *�����ȯ�Զ�ʬ
           03  PTINF-SKYPRTFLG               PIC  X(01).
      *ü���ɣ�
           03  PTINF-TERMID                  PIC  X(16).
      *���ڡ��졼���ɣ�
           03  PTINF-OPID                    PIC  X(16).
      *����ǯ����
           03  PTINF-CREYMD                  PIC  X(08).
      *����ǯ����
           03  PTINF-UPYMD                   PIC  X(08).
      *��������
           03  PTINF-UPHMS                   PIC  X(06).
      *virtual����
           03  PTINF-ST-SRYYMD               PIC  X(08).
           03  PTINF-ED-SRYYMD               PIC  X(08).
           03  PTINF-NYUGAIKBN               PIC  X(01).
           03  PTINF-HKNKBN                  PIC  X(01).
           03  PTINF-SRYYMD                  PIC  X(08).
           03  PTINF-SRYYM                   PIC  X(06).
           03  PTINF-HKNKBN2                 PIC  X(01).
           03  PTINF-VPTNUM                  PIC  X(20).
