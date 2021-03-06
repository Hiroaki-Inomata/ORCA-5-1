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
      ******************************************************************
      *    ���Уɥ쥹�ݥ󥹴�����Ͽ�����Ź԰٥����ƥ�����������
      *                                 COPY (CPAPISYSDAY01V2RES.INC)
      ******************************************************************
        01  XML-SYSTEMDAILYRES.
            02  SYSDAYRES-CONTEXT            PIC S9(9)   BINARY.
            02  SYSDAYRES-OBJECT             PIC X(36).
            02  SYSDAYRES-MODE               PIC S9(9)   BINARY.
            02  SYSDAYRES-RECORDNAME         PIC X(128).
            02  SYSDAYRES-APPOINTRES.
      *       �»���
              03  SYSDAYRES-INFORMATION-DATE    PIC X(10).
      *       �»ܻ���
              03  SYSDAYRES-INFORMATION-TIME    PIC X(8).
      *       �ֵѥ�����
              03  SYSDAYRES-API-RESULT          PIC X(2).
      *       �ֵѥ�å�����
              03  SYSDAYRES-API-RESULT-MSG      PIC X(100).
      *
              03  SYSDAYRES-RESKEY              PIC X(100).
      *       �����
              03  SYSDAYRES-BASE-DATE           PIC X(10).
      *       ������Ͽ�ط�
              03  SYSDAYRES-PATIENT-INFOMATION.
      *           �����Ͽ���������å���ʬ
                  05  SYSDAYRES-JYUFUKU-CHKKBN     PIC X(01).
                  05  SYSDAYRES-JYUFUKU-CHK-MEI    PIC X(40).
      *           ͹���ֹ漫ư���ܶ�ʬ
                  05  SYSDAYRES-POSTKBN-FLG        PIC X(01).
                  05  SYSDAYRES-POSTKBN-MEI        PIC X(40).
      *           ��ƻ�ܸ�̾���ܶ�ʬ
                  05  SYSDAYRES-KENMEIKBN-FLG      PIC X(01).
                  05  SYSDAYRES-KENMEIKBN-MEI      PIC X(40).
      *
      *       ���Ź԰ٴط�
              03  SYSDAYRES-MEDICAL-INFOMATION.
      *����
      *SYS1001
                  05  SYSDAYRES-SET-INF.
      *           ������ʬ
                      07  SYSDAYRES-SYOHOKBN           PIC X(01).
                      07  SYSDAYRES-SYOHOKBN-MEI       PIC X(40).
      *           ������ü������
                      07  SYSDAYRES-TAX-HASU-KBN       PIC X(01).
                      07  SYSDAYRES-TAX-HASU-MEI       PIC X(40).
      *           ���񥳡���ü������
                      07  SYSDAYRES-JIHI-HASU-KBN      PIC X(01).
                      07  SYSDAYRES-JIHI-HASU-MEI      PIC X(40).
      *SYS1038
      *           �鲼�����������Ѵ�����
                      07  SYSDAYRES-CHG310FLG          PIC X(01).
                      07  SYSDAYRES-CHG310FLG-MEI      PIC X(40).
      *           �������Ŷ�ʬ�Ѵ�����
                      07  SYSDAYRES-CHGSRYSYU-FLG      PIC X(01).
                      07  SYSDAYRES-CHGSRYSYU-MEI      PIC X(40).
      *           ���å�Ÿ����ʬ
                      07  SYSDAYRES-SETALLKBN          PIC X(01).
                      07  SYSDAYRES-SETALLKBN-MEI      PIC X(40).
      *
      *�ꥯ�����ȡᣰ��
                  05  SYSDAYRES-SET01-INF.
      *SYS1008
      *           �ǻ�����ưȯ��
                      07  SYSDAYRES-INITAUTO-FLG       PIC X(01).
                      07  SYSDAYRES-INITAUTO-MEI       PIC X(40).
      *           ������ܲû���ư����
                      07  SYSDAYRES-IKUJIEIYO-FLG      PIC X(01).
                      07  SYSDAYRES-IKUJIEIYO-MEI      PIC X(40).
      *           ������ܲû���ư�����
                      07  SYSDAYRES-IKUJIEIYO-SRYKA    PIC X(02).
                      07  SYSDAYRES-IKUJIEIYO-KAMEI    PIC X(40).
      *           ��̾������ʬ����μ�ưȯ��
                      07  SYSDAYRES-BYOMEIAUTO-FLG     PIC X(01).
                      07  SYSDAYRES-BYOMEIAUTO-MEI     PIC X(40).
      *SYS1007
      *           ����ô������
                      07  SYSDAYRES-RYOYOU-FLG         PIC X(01).
                      07  SYSDAYRES-RYOYOU-MEI         PIC X(40).
      *SYS1008
      *           ��������û������å�
                      07  SYSDAYRES-GAIKANRIKBN-CHK    PIC X(01).
                      07  SYSDAYRES-GAIKANRIKBN-MEI    PIC X(40).
      *SYS1008
      *           ���ֳ��û��ʾ����������
                      07  SYSDAYRES-JIKANTOKU-NYU-FLG  PIC X(01).
                      07  SYSDAYRES-JIKANTOKU-NYU-MEI  PIC X(40).
      *
      *�ꥯ�����ȡᣰ��
      *           �ǽ��象������δ���
                      07  SYSDAYRES-LAST-KIKAN         PIC X(02).
      *           �����ݸ��ȹ礻�������å�
                      07  SYSDAYRES-ZENHKNCHK-FLG      PIC X(01).
                      07  SYSDAYRES-ZENHKNCHK-MEI      PIC X(40).
      *
      *�ꥯ�����ȡᣰ��������
                  05  SYSDAYRES-SET02-INF.
      *SYS1008
      *           ������Ϳ�̥����å�
                      07  SYSDAYRES-TOUYOCHK-FLG       PIC X(01).
                      07  SYSDAYRES-TOUYOCHK-MEI       PIC X(40).
      *           ����������Ķ�����å�
                      07  SYSDAYRES-SHOHOU30CHK        PIC X(01).
                      07  SYSDAYRES-SHOHOU30CHK-MEI    PIC X(40).
      *
      *           ���޾������������å�
                      07  SYSDAYRES-YAKJYOCHK-FLG      PIC X(01).
                      07  SYSDAYRES-YAKJYOCHK-MEI      PIC X(40).
      *           ��Ģ�û���ǧ�����å�
                      07  SYSDAYRES-TECHOKBN-FLG       PIC X(01).
                      07  SYSDAYRES-TECHOKBN-MEI       PIC X(40).
      *           ���ֳ��۵ޱ�������å�
                      07  SYSDAYRES-JGAIKEIKBN-FLG     PIC X(01).
                      07  SYSDAYRES-JGAIKEIKBN-MEI     PIC X(40).
      *
      *SYS1038
      *           ������ˡ����̤���û���ʬ
                      07  SYSDAYRES-SEI20KSNKBN        PIC X(01).
                      07  SYSDAYRES-SEI20KSNKBN-MEI    PIC X(40).
      *           ������ˡ����̤���û���ʬ
                      07  SYSDAYRES-SIN20KSNKBN        PIC X(01).
                      07  SYSDAYRES-SIN20KSNKBN-MEI    PIC X(40).
      *           �����Ѵ���ʬ
                      07  SYSDAYRES-HAIKIKBN           PIC X(01).
                      07  SYSDAYRES-HAIKIKBN-MEI       PIC X(40).
      *SYS1007
      *           Ĵ�޵��Ѵ�����
                      07  SYSDAYRES-CHOKI-FLG          PIC X(01).
                      07  SYSDAYRES-CHOKI-MEI          PIC X(40).
      *           ���޾��������ʰ��̡�
                      07  SYSDAYRES-YAKJYO1-FLG        PIC X(01).
                      07  SYSDAYRES-YAKJYO1-MEI        PIC X(40).
      *           ���޾���������Ϸ�͡�
                      07  SYSDAYRES-YAKJYO2-FLG        PIC X(01).
                      07  SYSDAYRES-YAKJYO2-MEI        PIC X(40).
      *           �������޼��Ŵ�����̵ͭ
                      07  SYSDAYRES-TOKUTEI-FLG        PIC X(01).
                      07  SYSDAYRES-TOKUTEI-MEI        PIC X(40).
      *
      *           �������μ�ưȯ���ʳ����
                      07  SYSDAYRES-TEISEIAUTO-FLG     PIC X(01).
                      07  SYSDAYRES-TEISEIAUTO-MEI     PIC X(40).
      *           �������μ�ưȯ���������������������
                      07  SYSDAYRES-TEISEITOKU-FLG     PIC X(01).
                      07  SYSDAYRES-TEISEITOKU-MEI     PIC X(40).
      *           ���θ����û���ưȯ��
                      07  SYSDAYRES-KENSAAUTO-FLG      PIC X(01).
                      07  SYSDAYRES-KENSAAUTO-MEI      PIC X(40).
      *           �������Ǵ����û����ʼ̿����ǡ�
                      07  SYSDAYRES-GAZOUKSN1-FLG      PIC X(01).
                      07  SYSDAYRES-GAZOUKSN1-MEI      PIC X(40).
      *
      *�ꥯ�����ȡᣰ��
                  05  SYSDAYRES-SET04-INF.
      *           ����μ谷����ʬ
                      07  SYSDAYRES-NYKNKNRKBN          PIC X(01).
                      07  SYSDAYRES-NYKNKNRKBN-MEI      PIC X(40).
      *           ���⡦�ֶ�������ʬ
                      07  SYSDAYRES-NYKHNKKBN           PIC X(01).
                      07  SYSDAYRES-NYKHNKKBN-MEI       PIC X(40).
      *           �������������
                      07  SYSDAYRES-TEIKINKBN           PIC X(01).
                      07  SYSDAYRES-TEIKINKBN-MEI       PIC X(40).
      *
