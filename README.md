<h1 align="center">
  <br>
  🖥️ HelloOS
  <br>
</h1>

<p align="center">
  <b>Операционная система на Assembly x86/x32, написанная с нуля на FASM — ради понимания того, как всё это вообще работает.</b>
</p>

![FASM](https://img.shields.io/badge/FASM-Flat%20Assembler-blue?style=flat-square&logo=assemblyscript&logoColor=white)
![x86](https://img.shields.io/badge/CPU-x86%20%7C%20x32-purple?style=flat-square&logo=intel&logoColor=white)
![FAT16](https://img.shields.io/badge/FS-FAT16-orange?style=flat-square)
![Real Mode](https://img.shields.io/badge/16--bit-Real%20Mode-green?style=flat-square)
![Protected Mode](https://img.shields.io/badge/32--bit-Protected%20Mode-red?style=flat-square)
![Bochs](https://img.shields.io/badge/emulator-Bochs-lightgrey?style=flat-square)
![QEMU](https://img.shields.io/badge/emulator-QEMU-red?style=flat-square&logo=qemu&logoColor=white)
![GitHub stars](https://img.shields.io/github/stars/k0lan4ik/HelloOS?style=flat-square&logo=github)
![GitHub forks](https://img.shields.io/github/forks/k0lan4ik/HelloOS?style=flat-square&logo=github)
![Repo size](https://img.shields.io/github/repo-size/k0lan4ik/HelloOS?style=flat-square)
![Last commit](https://img.shields.io/github/last-commit/k0lan4ik/HelloOS?style=flat-square)
![License](https://img.shields.io/github/license/k0lan4ik/HelloOS?style=flat-square)

---

## 📖 О проекте

**HelloOS** — самописная ОС на чистом ассемблере ([FASM](https://flatassembler.net/)), созданная с образовательной целью. Проект существует в **двух версиях**:

- **16-bit** — классическая Real Mode ОС с загрузчиком под FAT16 (`KERNEL/` + `Boot.ASM`)
- **32-bit** — Protected Mode ядро с переключением в PM, своими драйверами и потоками (`KERNEL_X32/`)

> Цель — разобраться в принципах работы загрузчиков, переключения режимов процессора, файловых систем, прерываний и управления памятью на самом низком уровне.

---

## 🗂️ Версии ОС

### 16-bit (Real Mode)

Первая версия ОС — классическая 16-битная система, работающая в реальном режиме процессора.

| Файл | Описание |
|---|---|
| `Boot.ASM` | Первичный загрузчик (MBR, 512 байт, FAT16) |
| `BootLoad.ASM` | Вторичный загрузчик, загружает ядро с диска |
| `KERNEL/KERNEL.asm` | Точка входа в ядро |
| `KERNEL/FILESYS.asm` | Драйвер файловой системы FAT16 |
| `KERNEL/INTERPUT.asm` | Обработчики прерываний (IDT) |
| `KERNEL/MEMORY.asm` | Базовое управление памятью |

### 32-bit (Protected Mode)

Вторая версия — переход в Protected Mode с полноценной архитектурой ядра. Образ диска собирается через `MakeImage.asm`.

| Файл / Папка | Описание |
|---|---|
| `KERNEL_X32/KERNEL.asm` | Главный файл ядра x32 |
| `KERNEL_X32/Interrupt.asm` | Таблица прерываний IDT (PM) |
| `KERNEL_X32/Interrupt/` | Обработчики отдельных прерываний |
| `KERNEL_X32/Memory/` | Управление памятью в PM |
| `KERNEL_X32/Drivers/` | Драйверы устройств |
| `KERNEL_X32/Threads/` | Поддержка потоков |
| `KERNEL_X32/ScreenMode03.asm` | Вывод на экран (текстовый режим 03h) |
| `KERNEL_X32/Timer.asm` | Таймер (PIT) |
| `KERNEL_X32/Input.asm` | Обработка ввода |
| `MakeImage.asm` | Сборка образа диска (FASM) |

---

## 📁 Структура проекта

```
HelloOS/
│
├── Boot.ASM              # Первичный загрузчик (MBR)
├── BootLoad.ASM          # Вторичный загрузчик
├── MakeImage.asm         # Скрипт сборки образа диска (FASM)
├── bochsrc.bxrc          # Конфиг эмулятора Bochs
│
├── KERNEL/               # 16-bit ядро (Real Mode)
│   ├── KERNEL.asm
│   ├── FILESYS.asm
│   ├── FileSysStruct.inc
│   ├── INTERPUT.asm
│   └── MEMORY.asm
│
├── KERNEL_X32/           # 32-bit ядро (Protected Mode)
│   ├── KERNEL.asm
│   ├── KERNEL.SYS
│   ├── Structs.asm
│   ├── Blocks.inc
│   ├── Interrupt.asm
│   ├── Interrupt/
│   ├── Memory/
│   ├── Drivers/
│   ├── Threads/
│   ├── ScreenMode03.asm
│   ├── Timer.asm
│   └── Input.asm
│
├── EXPLORERCAM/          # Файловый менеджер (в разработке)
├── DIV0.asm              # Обработчик деления на ноль
├── INVOP.asm             # Обработчик недопустимого опкода
├── EGA#.asm              # Утилита EGA
└── EGA.CPI               # Кодовая страница EGA
```

---

## 🚀 Сборка и запуск

### Требования

- [FASM](https://flatassembler.net/) — ассемблер (Flat Assembler)
- [Bochs](https://bochs.sourceforge.io/) или [QEMU](https://www.qemu.org/) — эмулятор

### Сборка образа диска (32-bit)

```bash
fasm MakeImage.asm
```

### Запуск в Bochs

```bash
bochs -f bochsrc.bxrc
```

### Запуск в QEMU

```bash
qemu-system-i386 -hda HelloOS.img
```

---

## 🛠️ Технологии

| | 16-bit версия | 32-bit версия |
|---|---|---|
| **Ассемблер** | FASM | FASM |
| **Режим CPU** | Real Mode (16-bit) | Protected Mode (32-bit) |
| **Файловая система** | FAT16 | FAT16 |
| **Видео** | EGA/VGA текст | Текстовый режим 03h |
| **Ядро** | `KERNEL/` | `KERNEL_X32/` |

---

## 📜 Лицензия

Распространяется под лицензией **MIT**. Подробнее — в файле [LICENSE](LICENSE).

---

<p align="center">
  Сделано с ❤️ и кучей <code>xor eax, eax</code>
</p>
