/* -*- mode: c; c-file-style: "linux" -*-
 *  vi: set shiftwidth=8 tabstop=8 noexpandtab:
 *
 *  Copyright 2012 Elovalo project group 
 *  
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *  
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *  
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/* Set serial parameters using non-standard baud rate hack:
 * http://stackoverflow.com/questions/3192478/specifying-non-standard-baud-rate-for-ftdi-virtual-serial-port-under-linux
 * and
 * http://stackoverflow.com/questions/4968529/how-to-set-baud-rate-to-307200-on-linux */

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <termios.h>
#include <sys/ioctl.h>
#include <linux/serial.h>

int open_elovalo(const char *dev, int speed)
{
	// Open device
	int fd = open(dev, O_RDWR | O_NOCTTY);
	if (fd == -1) return -1;

	// Start with raw values
	struct termios term;
	term.c_cflag = B38400 | CS8 | CLOCAL | CREAD; 
	cfmakeraw(&term);
        if (tcsetattr(fd,TCSANOW,&term) == -1) return -1;

	// Then the hack
	struct serial_struct serial;
	if(ioctl(fd, TIOCGSERIAL, &serial) == -1) return -1;

	serial.flags = (serial.flags & ~ASYNC_SPD_MASK) | ASYNC_SPD_CUST;
	serial.custom_divisor = (serial.baud_base + (speed / 2)) / speed;
	
	if(ioctl(fd, TIOCSSERIAL, &serial) == -1) return -1;
	return fd;
}
